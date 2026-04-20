;;; .nixpkgs/.doom.d/ai-behaviors.el --- Transient menu for ai-behaviors hashtags -*- lexical-binding: t; -*-

(require 'transient)

;;; State
;; Invariant: +ai-behaviors--mode is nil or a single mode name string.
;; Invariant: +ai-behaviors--tags contains no duplicates.

(defvar +ai-behaviors--mode nil "Selected operating mode, or nil.")
(defvar +ai-behaviors--tags nil "List of selected quality/technique/modifier tag names.")

(defvar +ai-behaviors--presets
  '(("Frame" "#frame" "Frame")
    ("Research" "#research" "Research")
    ("Design" "#design" "Design")
    ("Spec" "#spec" "Spec")
    ("Code" "#code" "Code")
    ("Review" "#review" "Review")
    ("Test" "#test" "Test")
    ("Debug" "#debug" "Debug")
    ("Mentor" "#mentor" "Mentor")
    ("Impl" "#mpl" "Impl")
    ("Commit" "#commit" "Commit"))
  "List of preset entries (ID HASHTAGS DESCRIPTION).
Each entry defines a preset that inserts HASHTAGS directly.")

(defun +ai-behaviors--toggle-mode (name)
  "Toggle NAME as the selected mode. Deselects if already selected."
  (setq +ai-behaviors--mode (if (equal +ai-behaviors--mode name) nil name)))

(defun +ai-behaviors--toggle-tag (name)
  "Toggle NAME in the selected tags list."
  (if (member name +ai-behaviors--tags)
      (setq +ai-behaviors--tags (delete name +ai-behaviors--tags))
    (push name +ai-behaviors--tags)))

(defun +ai-behaviors--reset ()
  "Clear all selections."
  (setq +ai-behaviors--mode nil +ai-behaviors--tags nil))

(defun +ai-behaviors--format ()
  "Format current selection as a hashtag string for insertion.
Post: returns empty string when nothing selected, otherwise
space-separated hashtags with mode first."
  (let ((parts (append (when +ai-behaviors--mode
                         (list (concat "#=" +ai-behaviors--mode)))
                       (mapcar (lambda (tag) (concat "#" tag))
                               (reverse +ai-behaviors--tags)))))
    (string-join parts " ")))

;;; Suffix generators

(defmacro +ab--mode (sym name desc)
  "Define a transient suffix SYM that toggles mode NAME with DESC."
  (declare (indent 1))
  `(transient-define-suffix ,sym ()
     :transient t
     :description (lambda ()
                    (concat (if (equal +ai-behaviors--mode ,name)
                                (propertize "●" 'face 'transient-argument)
                              (propertize "○" 'face 'shadow))
                            ,(format " #=%s — %s" name desc)))
     (interactive)
     (+ai-behaviors--toggle-mode ,name)))

(defmacro +ab--tag (sym name desc)
  "Define a transient suffix SYM that toggles tag NAME with DESC."
  (declare (indent 1))
  `(transient-define-suffix ,sym ()
     :transient t
     :description (lambda ()
                    (concat (if (member ,name +ai-behaviors--tags)
                                (propertize "●" 'face 'transient-argument)
                              (propertize "○" 'face 'shadow))
                            ,(format " #%s — %s" name desc)))
     (interactive)
     (+ai-behaviors--toggle-tag ,name)))

(defun +ai-behaviors--preset-suffixes ()
  "Generate transient suffix specs for all presets.
Returns a list suitable for use in a transient group."
  (let ((keys "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
        (result nil)
        (idx 0))
    (dolist (preset +ai-behaviors--presets)
      (let* ((id (nth 0 preset))
             (tags (nth 1 preset))
             (desc (nth 2 preset))
             (key (if (< idx (length keys))
                      (string (aref keys idx))
                    (format "M-%c" (aref keys (- idx (length keys))))))
             (sym (intern (format "+ab-p/%s" id))))
        (eval `(transient-define-suffix ,sym ()
                 :description ,(format "%s — %s" desc
                                       (propertize tags 'face 'shadow))
                 (interactive)
                 (insert ,tags " ")
                 (+ai-behaviors--reset)))
        (push (list key sym) result)
        (cl-incf idx)))
    (nreverse result)))

(progn
;;; Mode suffixes — one only, exclusive selection

  (+ab--mode +ab-m/frame     "frame"    "Def problem")
  (+ab--mode +ab-m/research  "research" "Facts")
  (+ab--mode +ab-m/design    "design"   "Solutions?")
  (+ab--mode +ab-m/spec      "spec"     "Plan")
  (+ab--mode +ab-m/code      "code"     "c")
  (+ab--mode +ab-m/debug     "debug"    "Root cause")
  (+ab--mode +ab-m/review    "review"   "r")
  (+ab--mode +ab-m/test      "test"     "t")
  (+ab--mode +ab-m/drive     "drive"    "You steer")
  (+ab--mode +ab-m/navigate  "navigate" "LLM steer")
  (+ab--mode +ab-m/record    "record"   "Documenting")
  (+ab--mode +ab-m/mentor    "mentor"   "Learn")
  (+ab--mode +ab-m/probe     "probe"    "Ask question")

;;; Quality suffixes — stack freely, each independent axis

  (+ab--tag +ab-q/deep             "deep"             "Vertical")
  (+ab--tag +ab-q/wide             "wide"             "Horizontal")
  (+ab--tag +ab-q/ground           "ground"           "Fact check")
  (+ab--tag +ab-q/negative-space   "negative-space"   "Missing?")
  (+ab--tag +ab-q/challenge        "challenge"        "Correct?")
  (+ab--tag +ab-q/steel-man        "steel-man"        "Perfect?")
  (+ab--tag +ab-q/user-lens        "user-lens"        "End user")
  (+ab--tag +ab-q/concise          "concise"          "Density")
  (+ab--tag +ab-q/first-principles "first-principles" "Reasoning")
  (+ab--tag +ab-q/creative         "creative"         "Solution space")
  (+ab--tag +ab-q/subtract         "subtract"         "Less UI")
  (+ab--tag +ab-q/meta             "meta"             "Reflect")

;;; Technique suffixes — orthogonal cognitive methods

  (+ab--tag +ab-t/simulate   "simulate"  "Mental execution")
  (+ab--tag +ab-t/decompose  "decompose" "Structural division")
  (+ab--tag +ab-t/recursive  "recursive" "Self-application")
  (+ab--tag +ab-t/fractal    "fractal"   "Scale variation")
  (+ab--tag +ab-t/tdd        "tdd"       "Test-driven cycle")
  (+ab--tag +ab-t/io         "io"        "IO boundaries")
  (+ab--tag +ab-t/contract   "contract"  "Correctness criteria")
  (+ab--tag +ab-t/backward   "backward"  "Reverse reasoning")
  (+ab--tag +ab-t/analogy    "analogy"   "Structural transfer")
  (+ab--tag +ab-t/temporal   "temporal"  "Ordering analysis")
  (+ab--tag +ab-t/name       "name"      "Naming precision")
  (+ab--tag +ab-t/checklist  "checklist" "Scope tracking")

;;; Modifier suffix

  (+ab--tag +ab-o/file "file" "Persist output to file"))

;;; Action suffixes

(transient-define-suffix +ab/insert ()
  :description (lambda ()
                 (let ((sel (+ai-behaviors--format)))
                   (if (string-empty-p sel) "Insert (nothing selected)"
                     (concat "Insert: " (propertize sel 'face 'transient-argument)))))
  (interactive)
  (let ((text (+ai-behaviors--format)))
    (unless (string-empty-p text)
      (insert text " "))
    (+ai-behaviors--reset)))

(transient-define-suffix +ab/explain ()
  :description "Insert with #EXPLAIN"
  (interactive)
  (let ((text (+ai-behaviors--format)))
    (insert "#EXPLAIN" (if (string-empty-p text) "" (concat " " text)) " ")
    (+ai-behaviors--reset)))

(transient-define-suffix +ab/clear ()
  :transient t
  :description "Clear selections"
  (interactive)
  (+ai-behaviors--reset))

;;; Prefix

(transient-define-prefix +ai-behaviors-menu ()
  "Select ai-behaviors hashtags to insert at point.
Three columns: modes (exclusive), qualities, techniques.
RET inserts the formatted hashtag string."
  [["Modes (one only)"
    ("f" +ab-m/frame)
    ("r" +ab-m/research)
    ("d" +ab-m/design)
    ("s" +ab-m/spec)
    ("c" +ab-m/code)
    ("b" +ab-m/debug)
    ("v" +ab-m/review)
    ("t" +ab-m/test)
    ("D" +ab-m/drive)
    ("n" +ab-m/navigate)
    ("R" +ab-m/record)
    ("m" +ab-m/mentor)
    ("p" +ab-m/probe)]
   ["Qualities"
    ("1" +ab-q/deep)
    ("2" +ab-q/wide)
    ("3" +ab-q/ground)
    ("4" +ab-q/negative-space)
    ("5" +ab-q/challenge)
    ("6" +ab-q/steel-man)
    ("7" +ab-q/user-lens)
    ("8" +ab-q/concise)
    ("9" +ab-q/first-principles)
    ("0" +ab-q/creative)
    ("-" +ab-q/subtract)
    ("=" +ab-q/meta)]
   ["Techniques"
    ("e" +ab-t/simulate)
    ("w" +ab-t/decompose)
    ("a" +ab-t/recursive)
    ("g" +ab-t/fractal)
    ("h" +ab-t/tdd)
    ("i" +ab-t/io)
    ("j" +ab-t/contract)
    ("k" +ab-t/backward)
    ("l" +ab-t/analogy)
    ("u" +ab-t/temporal)
    ("x" +ab-t/name)
    ("y" +ab-t/checklist)
    ""
    "Modifiers"
    ("!" +ab-o/file)]]
  ["Presets"
   :setup-children
   (lambda (_)
     (mapcar (pcase-lambda (`(,key ,sym))
               (transient-parse-suffix
                (oref transient--prefix command)
                (list key sym)))
             (+ai-behaviors--preset-suffixes)))]
  ["Actions"
   ("RET" +ab/insert)
   ("?" +ab/explain)
   ("C-k" +ab/clear)])

;;;###autoload
(defun +ai-behaviors-insert-tags ()
  "Open transient menu to select and insert ai-behaviors hashtags.
Pre: point is in a writable buffer.
Post: selected tags inserted at point, state reset."
  (interactive)
  (+ai-behaviors--reset)
  (+ai-behaviors-menu))

(provide 'ai-behaviors)
;;; ai-behaviors.el ends here
