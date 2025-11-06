;;; lint.el --- GPTEL lint tool -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, lint

;;; Commentary:
;;
;; Linting tool for GPTEL tools and buffers.
;;
;;; Code:

;;; Tool: Lint Buffer (gathers diagnostics/warnings/errors for a buffer)
(defun gptelt-lint-buffer (buffer-name &optional offset limit)
  "Get all linter diagnostics configured by user for BUFFER-NAME.

Returns diagnostics from user's chosen linters (LSP, Flycheck, Flymake, package-lint).
These are type errors, lint errors, and warnings configured per-file-type:
- Elisp: elisp-lint, package-lint
- TypeScript/TSX: TypeScript type checker, ESLint/Biome
- Python: mypy/pyright, ruff/flake8
- etc.

Use this AFTER making changes to verify code correctness.
Optional OFFSET and LIMIT for paging diagnostics (default offset=0, limit=50)."
  (let* ((buf (get-buffer buffer-name))
         (mode (and buf (buffer-local-value 'major-mode buf)))
         (results '()))
    (unless buf (error "Buffer not found: %s" buffer-name))
    (with-current-buffer buf
      (cond
       ((and (bound-and-true-p lsp-mode)
             (fboundp 'lsp-diagnostics))
        (let* ((file-path (buffer-file-name buf))
               (lsp-diags (ignore-errors (lsp-diagnostics t)))
               (file-diags (when (and lsp-diags file-path)
                             (gethash file-path lsp-diags))))
          (when file-diags
            (setq results
                  (append results
                          (list (list :source "lsp" :diagnostics file-diags)))))))
       ((and (featurep 'flycheck)
             (bound-and-true-p flycheck-mode))
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
                      (let ((src (plist-get item :source))
                            (diags (plist-get item :diagnostics)))
                        (if (hash-table-p diags)
                            ;; lsp-diagnostics returns hash table per file
                            (let ((file-diags '()))
                              (maphash (lambda (_file diag-list)
                                         (setq file-diags (append file-diags diag-list)))
                                       diags)
                              (mapcar (lambda (diag)
                                        (cons :source (cons src diag)))
                                      file-diags))
                          ;; other sources return list directly
                          (mapcar (lambda (diag)
                                    (cons :source (cons src diag)))
                                  diags))))
                    results)))
           (total (length flat-list))
           (start (or offset 0))
           (lim (or limit 50))
           (paged (seq-subseq flat-list start (min total (+ start lim)))))
      (list :total total :offset start :limit lim :diagnostics paged))))

(comment
  (gptelt-lint-buffer (current-buffer)))

;;; lint project
(defun gptelt-lint-project (&optional offset limit)
  "Get all linter diagnostics for the entire project.

Returns diagnostics from user's chosen linters across all project files.
Use this to get an overview of all issues in the project.
Optional OFFSET and LIMIT for paging diagnostics (default offset=0, limit=50)."
  (let ((results '()))
    (cond
     ;; LSP: Get project-wide diagnostics
     ((and (bound-and-true-p lsp-mode)
           (fboundp 'lsp-diagnostics))
      (let ((lsp-diags (ignore-errors (lsp-diagnostics t))))
        (when lsp-diags
          (setq results
                (append results
                        (list (list :source "lsp" :diagnostics lsp-diags)))))))
     ;; Flymake: Get project-wide diagnostics
     ((and (featurep 'flymake)
           (fboundp 'flymake--project-diagnostics))
      (when-let ((project (project-current)))
        (let ((diags (ignore-errors (flymake--project-diagnostics project))))
          (when diags
            (setq results (append results
                                  (list (list :source "flymake" :diagnostics diags))))))))
     ;; Flycheck: Collect from all project buffers
     ((featurep 'flycheck)
      (when-let ((project (project-current)))
        (let ((all-errs '()))
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (and (bound-and-true-p flycheck-mode)
                         (boundp 'flycheck-current-errors)
                         flycheck-current-errors
                         (project-current)
                         (equal (project-root (project-current))
                                (project-root project)))
                (setq all-errs (append all-errs flycheck-current-errors)))))
          (when all-errs
            (setq results (append results
                                  (list (list :source "flycheck" :diagnostics all-errs)))))))))
    ;; Flatten and page results
    (let* ((flat-list
            (apply #'append
                   (mapcar
                    (lambda (item)
                      (let ((src (plist-get item :source))
                            (diags (plist-get item :diagnostics)))
                        (if (hash-table-p diags)
                            ;; lsp-diagnostics returns hash table with all files
                            (let ((file-diags '()))
                              (maphash (lambda (_file diag-list)
                                         (setq file-diags (append file-diags diag-list)))
                                       diags)
                              (mapcar (lambda (diag)
                                        (cons :source (cons src diag)))
                                      file-diags))
                          ;; other sources return list directly
                          (mapcar (lambda (diag)
                                    (cons :source (cons src diag)))
                                  diags))))
                    results)))
           (total (length flat-list))
           (start (or offset 0))
           (lim (or limit 50))
           (paged (seq-subseq flat-list start (min total (+ start lim)))))
      (list :total total :offset start :limit lim :diagnostics paged))))

(comment
  (gptelt-lint-project))

;;; lint file
(defun gptelt-lint-file (file-path &optional offset limit)
  "Get all linter diagnostics configured by user for FILE-PATH.

Opens file if needed, then returns diagnostics from user's chosen linters.
Use this AFTER making changes to verify code correctness.
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
   :description "Get all diagnostics from user-configured linters for a buffer (Flycheck/Flymake/LSP/package-lint).

Returns type errors, lint errors, and warnings based on user's per-file-type linter setup:
- Elisp: elisp-lint, package-lint
- TypeScript/TSX/JavaScript: TypeScript compiler, ESLint/Biome
- Python: mypy/pyright, ruff/flake8
- Other languages: LSP diagnostics, language-specific linters

IMPORTANT: Use this tool AFTER completing file modifications to verify correctness.
Check this at the end of each task to ensure no errors were introduced."
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
   :category "lint"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "lint_file"
   :function #'gptelt-lint-file
   :description "Get all diagnostics from user-configured linters for a file by absolute path.

Returns type errors, lint errors, and warnings based on user's per-file-type linter setup.
Opens file if not already loaded.

IMPORTANT: Use this tool AFTER completing file modifications to verify correctness.
Check this at the end of each task to ensure no errors were introduced."
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
   :category "lint"
   :confirm nil
   :include t)
  (gptelt-make-tool
   :name "lint_project"
   :function #'gptelt-lint-project
   :description "Get all diagnostics from user-configured linters for the entire project.

Returns project-wide diagnostics from:
- LSP: All project diagnostics from LSP workspace
- Flymake: Project-wide diagnostics via flymake--project-diagnostics
- Flycheck: Collected from all project buffers with flycheck-mode enabled

Use this to get an overview of all issues across the project.
Optional OFFSET and LIMIT for paging diagnostics (default offset=0, limit=50)."
   :args '((:name "offset"
            :type integer
            :description "Start from this diagnostic (default 0)"
            :optional t)
           (:name "limit"
            :type integer
            :description "Number of diagnostics to return (default 50)"
            :optional t))
   :category "lint"
   :confirm nil
   :include t))

(provide 'lint)
;;; lint.el ends here
