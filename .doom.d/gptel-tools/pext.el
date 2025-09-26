;;; .nixpkgs/.doom.d/gptel-tools/pext.el -*- lexical-binding: t; -*-

;;; current tab content
(defun gptelt-pext-get-current-tab-content ()
  "Get the content of the current browser tab using pext."
  (interactive)
  (condition-case err
      (pext-post-message-sync "current-tab-content")
    (error (format "Error getting tab content: %s" (error-message-string err)))))

(comment
  (gptelt-pext-get-current-tab-content))

;;; register tools
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "browser_get_active_tab_content"
   :function #'gptelt-pext-get-current-tab-content
   :description "Get the content of the active browser tab."
   :args '()
   :category "browser"
   :confirm nil
   :include t))
