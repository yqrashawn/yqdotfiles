;;; gptel-claude-code-session.el --- Session management for Claude Code CLI  -*- lexical-binding: t; -*-

;; Author: yqrashawn
;; Keywords: tools, convenience

;;; Commentary:

;; Manages Claude Code session IDs per org heading via org properties.
;; Enables multi-turn conversation (--resume), branching (--fork-session)
;; when creating sibling headings, and model switching (--fork-session --model)
;; when the model changes between turns.

;;; Code:

(require 'cl-lib)
(require 'org-id)

;; Forward declarations -- gptel core
(defvar gptel-model)
(defvar gptel-org-branching-context)
(declare-function gptel--model-name "gptel")

;; Forward declarations -- org
(declare-function org-back-to-heading-or-point-min "org")
(declare-function org-set-property "org")
(declare-function org-entry-get "org")
(declare-function org-up-heading-safe "org")

;; Forward declarations -- stream module buffer-local vars
(defvar gptel-claude-code--session-id)
(defvar gptel-claude-code--session-model)

;;; Org property constants

(defconst gptel-claude-code--session-property "GPTEL_CLAUDE_CODE_SESSION"
  "Org property name for storing Claude Code session IDs.")

(defconst gptel-claude-code--session-model-property "GPTEL_CLAUDE_CODE_MODEL"
  "Org property name for storing the model used with a session.")

;;; Session property save/restore

(defun gptel-claude-code--save-session-property (session-id)
  "Save SESSION-ID as an org property on the current heading.
Also saves the current model name.  Does nothing outside `org-mode'."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (org-back-to-heading-or-point-min t)
     (org-set-property gptel-claude-code--session-property session-id)
     (org-set-property gptel-claude-code--session-model-property
                       (gptel--model-name gptel-model)))))

(defun gptel-claude-code--get-session-property ()
  "Read session-id from current heading's org property.
Returns nil if not found or not in `org-mode'."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (org-back-to-heading-or-point-min t)
     (org-entry-get nil gptel-claude-code--session-property))))

(defun gptel-claude-code--get-session-model-property ()
  "Read the model name from current heading's org property.
Returns nil if not found or not in `org-mode'."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (org-back-to-heading-or-point-min t)
     (org-entry-get nil gptel-claude-code--session-model-property))))

(defun gptel-claude-code--get-parent-session-property ()
  "Walk up the org heading tree looking for a session property.
Returns (SESSION-ID . MODEL) cons if found, nil otherwise."
  (when (derived-mode-p 'org-mode)
    (org-with-wide-buffer
     (org-back-to-heading-or-point-min t)
     (let ((found nil))
       (while (and (not found) (org-up-heading-safe))
         (let ((sid (org-entry-get nil gptel-claude-code--session-property))
               (model (org-entry-get nil gptel-claude-code--session-model-property)))
           (when sid
             (setq found (cons sid model)))))
       found))))

;;; Session state determination

(defun gptel-claude-code--session-state (_backend)
  "Determine the session state for the current request.

Returns a plist describing what kind of session to create:
  (:state :new :session-id \"new-uuid\")
  (:state :continue :session-id \"existing-uuid\")
  (:state :fork :parent-session-id \"parent-uuid\" :session-id \"new-uuid\")
  (:state :fork-model :session-id \"existing-uuid\"
          :new-session-id \"new-uuid\" :model \"new-model\")

_BACKEND is the gptel-claude-code backend (reserved for future use)."
  (let ((current-model (gptel--model-name gptel-model)))
    (if (and (derived-mode-p 'org-mode)
             (bound-and-true-p gptel-org-branching-context))
        ;; Org-mode with branching context
        (let ((session-id (gptel-claude-code--get-session-property))
              (stored-model (gptel-claude-code--get-session-model-property)))
          (cond
           ;; Current heading has a session
           ((and session-id stored-model
                 (equal stored-model current-model))
            (list :state :continue :session-id session-id))

           ;; Current heading has a session but model changed
           ((and session-id stored-model
                 (not (equal stored-model current-model)))
            (list :state :fork-model
                  :session-id session-id
                  :new-session-id (org-id-uuid)
                  :model current-model))

           ;; Current heading has session but no model recorded (legacy)
           (session-id
            (list :state :continue :session-id session-id))

           ;; No session on current heading, check parents
           (t
            (if-let* ((parent (gptel-claude-code--get-parent-session-property)))
                (list :state :fork
                      :parent-session-id (car parent)
                      :session-id (org-id-uuid))
              ;; No session anywhere
              (list :state :new :session-id (org-id-uuid))))))

      ;; Non-org-mode or no branching context
      (cond
       ;; Buffer-local session exists and model matches
       ((and gptel-claude-code--session-id
             gptel-claude-code--session-model
             (equal gptel-claude-code--session-model current-model))
        (list :state :continue :session-id gptel-claude-code--session-id))

       ;; Buffer-local session exists but model changed
       ((and gptel-claude-code--session-id
             gptel-claude-code--session-model
             (not (equal gptel-claude-code--session-model current-model)))
        (list :state :fork-model
              :session-id gptel-claude-code--session-id
              :new-session-id (org-id-uuid)
              :model current-model))

       ;; Buffer-local session exists but no model recorded (legacy)
       (gptel-claude-code--session-id
        (list :state :continue :session-id gptel-claude-code--session-id))

       ;; No session at all
       (t
        (list :state :new :session-id (org-id-uuid)))))))

;;; Session args builder

(defun gptel-claude-code--session-args (session-state)
  "Convert SESSION-STATE plist to Claude Code CLI arguments.

Returns a list of strings:
  :new        -> (\"--session-id\" \"uuid\")
  :continue   -> (\"--resume\" \"uuid\")
  :fork       -> (\"--resume\" \"parent-uuid\" \"--fork-session\"
                   \"--session-id\" \"new-uuid\")
  :fork-model -> (\"--resume\" \"uuid\" \"--fork-session\"
                   \"--session-id\" \"new-uuid\")"
  (pcase (plist-get session-state :state)
    (:new
     (list "--session-id" (plist-get session-state :session-id)))
    (:continue
     (list "--resume" (plist-get session-state :session-id)))
    (:fork
     (list "--resume" (plist-get session-state :parent-session-id)
           "--fork-session"
           "--session-id" (plist-get session-state :session-id)))
    (:fork-model
     (list "--resume" (plist-get session-state :session-id)
           "--fork-session"
           "--session-id" (plist-get session-state :new-session-id)))))

;;; Hook into init handler

(defun gptel-claude-code--on-session-init (session-id _model buffer)
  "Handle session initialization from Claude Code init message.

SESSION-ID is the session identifier from the init message.
_MODEL is the model name (already stored by the stream handler).
BUFFER is the chat buffer to operate in.

Saves the session-id as an org property if in `org-mode'."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (gptel-claude-code--save-session-property session-id))))

(provide 'gptel-claude-code-session)
;;; gptel-claude-code-session.el ends here
