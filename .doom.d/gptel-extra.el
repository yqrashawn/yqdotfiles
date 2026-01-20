;;; .nixpkgs/.doom.d/gptel-extra.el -*- lexical-binding: t; -*-

(defun +gptel--add-workspace-context ()
  "Add workspace context to gptel request params.
This sets buffer-local `gptel--request-params' with workspace metadata
that will be included in each gptel request."
  (setq-local
   gptel--request-params
   (list
    :metadata
    (list :workspace_root (or (++workspace-current-project-root) default-directory)
          :working_dir default-directory
          :project_name (when-let ((root (++workspace-current-project-root)))
                          (file-name-nondirectory
                           (directory-file-name root)))))))

(defun gptel--setup-workspace-context ()
  "Setup workspace context for gptel buffers.
Adds hook to set workspace context before each request."
  ;; Add to gptel-mode-hook for chat buffers
  (add-hook! 'gptel-mode-hook #'+gptel--add-workspace-context)
  
  ;; Also add to gptel-post-request-hook to refresh context for subsequent requests
  (add-hook! 'gptel-post-request-hook #'+gptel--add-workspace-context))
;; 
;; Initialize workspace context setup
(gptel--setup-workspace-context)


