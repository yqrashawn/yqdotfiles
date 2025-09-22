;;; lint.el --- GPTEL lint tool -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, lint

;;; Commentary:
;;
;; Linting tool for GPTEL tools and buffers.
;;
;;; Code:

;;; Tool: Lint Buffer (gathers diagnostics/warnings/errors for a buffer)
(defun gptelt-lint-buffer (buffer-name &optional offset limit)
  "Get lint diagnostics for BUFFER-NAME as a structured list. Supports LSP, Flycheck, Flymake, and package-lint for elisp. (Docstring limited to 80 chars width.)
Optional OFFSET and LIMIT for paging diagnostics (default offset=0, limit=50)."
  (let* ((buf (get-buffer buffer-name))
         (mode (and buf (buffer-local-value 'major-mode buf)))
         (results '()))
    (unless buf (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buf
      (cond
       ((and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-diagnostics))
        (let ((lsp-diags (ignore-errors (lsp-diagnostics t))))
          (when lsp-diags
            (setq results
                  (append results
                          (list (list :source "lsp" :diagnostics lsp-diags)))))))
       ((and (featurep 'flycheck) (bound-and-true-p flycheck-mode))
        (let ((errs (and (boundp 'flycheck-current-errors)
                         flycheck-current-errors)))
          (when errs
            (setq results (append results
                                  (list (list :source "flycheck" :diagnostics errs)))))))
       ((and (featurep 'flymake) (bound-and-true-p flymake-mode))
        (let ((diags (and (fboundp 'flymake-diagnostics) (flymake-diagnostics))))
          (when diags
            (setq results (append results (list (list :source "flymake" :diagnostics diags)))))))
       ((eq mode 'emacs-lisp-mode)
        (when (require 'package-lint nil t)
          (let ((problems (ignore-errors (package-lint-buffer buf))))
            (when (and problems (not (equal problems '())))
              (setq results (append results (list (list :source "package-lint" :diagnostics problems))))))))))
    ;; Flatten and page results
    (let* ((flat-list
            (apply #'append
                   (mapcar
                    (lambda (item)
                      (setq jjjitem item)
                      (setq item jjjitem)
                      (let ((src (plist-get item :source))
                            (diags (plist-get item :diagnostics)))
                        (ht-map (lambda (k diag)
                                  (cons :source (cons k diag)))
                                diags)))
                    results)))
           (total (length flat-list))
           (start (or offset 0))
           (lim (or limit 50))
           (paged (seq-subseq flat-list start (min total (+ start lim)))))
      (list :total total :offset start :limit lim :diagnostics paged))))

(comment
  (gptelt-lint-buffer (current-buffer)))

;;; lint file
(defun gptelt-lint-file (file-path &optional offset limit)
  "Get lint diagnostics for FILE-PATH. Opens the file if not already loaded and reuses `gptelt-lint-buffer`.
Optional OFFSET and LIMIT for paging diagnostics (default offset=0, limit=50)."
  (let* ((file-path (expand-file-name file-path))
         (buf (or (get-file-buffer file-path)
                  (when (file-exists-p file-path)
                    (find-file-noselect file-path)))))
    (unless buf (error "File not found: %s" file-path))
    (gptelt-lint-buffer (buffer-name buf) offset limit)))

(comment
  (gptelt-lint-file "~/workspace/home/miniser/src/clj/yqrashawn/miniser/module/copying_news/helpers.clj"))

;;; register tools
(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "lint_buffer"
   :function #'gptelt-lint-buffer
   :description "Lint a file (by name) and return all available diagnostics from various linters."
   :args '((:name "buffer_name"
            :type string
            :description "The name of the buffer to lint.")
           (:name "offset"
            :type integer
            :description "Start from this diagnostic (default 0)"
            :optional t)
           (:name "limit"
            :type integer
            :description "Number of diagnostics to return (default 50)"
            :optional t))
   :category "emacs"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "lint_file"
   :function #'gptelt-lint-file
   :description "Lint a file (by absolute path) and return all available diagnostics from various linters."
   :args '((:name "file_path"
            :type string
            :description "Absolute path to the file to lint.")
           (:name "offset"
            :type integer
            :description "Start from this diagnostic (default 0)"
            :optional t)
           (:name "limit"
            :type integer
            :description "Number of diagnostics to return (default 50)"
            :optional t))
   :category "emacs"
   :confirm nil
   :include t))
