;;; workspace.el --- GPTEL workspace analysis tools -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, workspace, project

;;; Commentary:
;;
;; Workspace and project analysis tools for GPTEL.
;; Provides intelligent repository overviews optimized for LLM consumption.
;;
;;; Code:

(require 'json)
(require 'cl-lib)

;;; Project Detection and Basic Info

(defun gptelt--get-project-root ()
  "Get the current project root directory."
  (or (when (fboundp '++workspace-current-project-root)
        (++workspace-current-project-root))
      (when (fboundp 'project-current)
        (when-let ((project (project-current)))
          (if (fboundp 'project-root)
              (project-root project)
            (car (project-roots project)))))
      default-directory))

(defun gptelt--detect-project-type (root-dir)
  "Detect project type based on files in ROOT-DIR."
  (let ((files (directory-files root-dir))
        (types '()))
    ;; Language specific detection
    (when (or (member "package.json" files)
              (member "yarn.lock" files)
              (member "pnpm-lock.yaml" files))
      (push "javascript/typescript" types))
    (when (or (member "deps.edn" files)
              (member "project.clj" files)
              (member "shadow-cljs.edn" files))
      (push "clojure/clojurescript" types))
    (when (or (member "go.mod" files)
              (member "go.sum" files))
      (push "go" types))
    (when (or (member "requirements.txt" files)
              (member "pyproject.toml" files)
              (member "setup.py" files)
              (member "Pipfile" files))
      (push "python" types))
    (when (or (member "Cargo.toml" files)
              (member "Cargo.lock" files))
      (push "rust" types))
    (when (or (member "pom.xml" files)
              (member "build.gradle" files))
      (push "java" types))

    ;; Framework detection
    (when (member "next.config.js" files)
      (push "next.js" types))
    (when (member "vue.config.js" files)
      (push "vue.js" types))
    (when (member "angular.json" files)
      (push "angular" types))

    (if types
        (string-join types ", ")
      "unknown")))

(defun gptelt--is-git-repo (dir)
  "Check if DIR is a git repository."
  (file-exists-p (expand-file-name ".git" dir)))

(defun gptelt--get-git-info (root-dir)
  "Get git information for ROOT-DIR."
  (when (gptelt--is-git-repo root-dir)
    (let ((default-directory root-dir))
      (list
       :is-git t
       :branch (string-trim
                (shell-command-to-string "git branch --show-current 2>/dev/null || echo 'unknown'"))
       :remote (string-trim
                (shell-command-to-string "git remote get-url origin 2>/dev/null || echo 'none'"))
       :commit-count (string-to-number
                      (shell-command-to-string "git rev-list --count HEAD 2>/dev/null || echo '0'"))
       :last-commit (string-trim
                     (shell-command-to-string "git log -1 --format='%h %s' 2>/dev/null || echo 'none'"))))))

;;; File Tree Generation

(defun gptelt--should-exclude-path (path excludes)
  "Check if PATH should be excluded based on EXCLUDES patterns."
  (cl-some (lambda (pattern)
             (string-match-p pattern path))
           excludes))

(defun gptelt--get-file-tree (root-dir &optional max-depth max-files)
  "Generate a file tree for ROOT-DIR with optional limits."
  (let* ((max-depth (or max-depth 4))
         (max-files (or max-files 200))
         (excludes '("\\.git/" "node_modules/" "\\.next/" "dist/" "build/"
                     "target/" "out/" "coverage/" "\\.venv/" "venv/"
                     "vendor/" "\\.cache/" "tmp/" "logs/" "__pycache__/"
                     "\\.pytest_cache/" "\\.cljs_build/"))
         (files '())
         (file-count 0))

    (cl-labels ((walk-directory (dir depth prefix)
                  (when (and (< depth max-depth) (< file-count max-files))
                    (let* ((entries (ignore-errors
                                      (directory-files-and-attributes
                                       dir nil "^[^.]" t)))
                           (dirs (cl-remove-if-not
                                  (lambda (entry) (eq (cadr entry) t)) entries))
                           (files-list (cl-remove-if
                                        (lambda (entry) (eq (cadr entry) t)) entries)))

                      ;; Add files first
                      (dolist (file (cl-subseq files-list 0 (min (length files-list)
                                                                 (- max-files file-count))))
                        (let ((rel-path (file-relative-name
                                         (expand-file-name (car file) dir) root-dir)))
                          (unless (gptelt--should-exclude-path rel-path excludes)
                            (push (format "%s%s" prefix (car file)) files)
                            (cl-incf file-count))))

                      ;; Then recurse into directories
                      (dolist (dir-entry (cl-subseq dirs 0 (min (length dirs) 10)))
                        (let* ((dir-name (car dir-entry))
                               (full-path (expand-file-name dir-name dir))
                               (rel-path (file-relative-name full-path root-dir)))
                          (unless (gptelt--should-exclude-path rel-path excludes)
                            (push (format "%s%s/" prefix dir-name) files)
                            (cl-incf file-count)
                            (walk-directory full-path
                                            (1+ depth)
                                            (concat prefix "  ")))))))))

      (walk-directory root-dir 0 ""))
    (nreverse files)))

;;; Hot Files Analysis (Git-based)

(defun gptelt--get-hot-files (root-dir &optional limit months)
  "Get files with most changes in the last MONTHS (default 12)."
  (let* ((limit (or limit 30))
         (months (or months 12))
         (default-directory root-dir))
    (when (gptelt--is-git-repo root-dir)
      (let ((output (shell-command-to-string
                     (format "git log --since=\"%d months ago\" --pretty=format: --name-only | grep -E '\\.' | grep -Ev '(node_modules|.git|dist|build|target|.next|__pycache__|coverage)' | sort | uniq -c | sort -nr | head -n %d"
                             months limit))))
        (when (and output (not (string-empty-p output)))
          (mapcar (lambda (line)
                    (when (string-match "^\\s-*\\([0-9]+\\)\\s-+\\(.+\\)$" line)
                      (list :file (match-string 2 line)
                            :changes (string-to-number (match-string 1 line)))))
                  (split-string output "\n" t)))))))

;;; Dependencies Analysis

(defun gptelt--extract-package-json-deps (file)
  "Extract dependencies from package.json FILE."
  (when (file-exists-p file)
    (condition-case nil
        (let* ((json-data (json-read-file file))
               (deps (append (cdr (assoc 'dependencies json-data))
                             (cdr (assoc 'devDependencies json-data))))
               (scripts (cdr (assoc 'scripts json-data))))
          (list :dependencies (mapcar (lambda (dep)
                                        (format "%s@%s" (car dep) (cdr dep)))
                                      deps)
                :scripts (mapcar (lambda (script)
                                   (format "%s: %s" (car script)
                                           (if (> (length (cdr script)) 80)
                                               (concat (substring (cdr script) 0 77) "...")
                                             (cdr script))))
                                 scripts)))
      (error nil))))

(defun gptelt--extract-deps-edn (file)
  "Extract dependencies from deps.edn FILE."
  (when (file-exists-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((deps '()))
            (goto-char (point-min))
            (while (re-search-forward "\\([a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+\\)\\s-+{:mvn/version\\s-+\"\\([^\"]+\\)\"}" nil t)
              (push (format "%s@%s" (match-string 1) (match-string 2)) deps))
            (goto-char (point-min))
            (while (re-search-forward "\\([a-zA-Z0-9_.-]+/[a-zA-Z0-9_.-]+\\)\\s-+{:git/tag\\s-+\"\\([^\"]+\\)\"}" nil t)
              (push (format "%s@%s" (match-string 1) (match-string 2)) deps))
            (list :dependencies (nreverse deps))))
      (error nil))))

(defun gptelt--extract-go-mod (file)
  "Extract dependencies from go.mod FILE."
  (when (file-exists-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((deps '()))
            (goto-char (point-min))
            (while (re-search-forward "^\\s-*\\([a-zA-Z0-9_.-]+\\(?:/[a-zA-Z0-9_.-]+\\)*\\)\\s-+\\(v[0-9.]+\\(?:-[a-zA-Z0-9]+\\)?\\)" nil t)
              (push (format "%s@%s" (match-string 1) (match-string 2)) deps))
            (list :dependencies (nreverse deps))))
      (error nil))))

(defun gptelt--extract-requirements-txt (file)
  "Extract dependencies from requirements.txt FILE."
  (when (file-exists-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((deps '()))
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (string-trim (buffer-substring-no-properties
                                        (line-beginning-position)
                                        (line-end-position)))))
                (when (and (not (string-empty-p line))
                           (not (string-prefix-p "#" line))
                           (not (string-prefix-p "-" line)))
                  (push line deps)))
              (forward-line 1))
            (list :dependencies (nreverse deps))))
      (error nil))))

(defun gptelt--get-project-dependencies (root-dir)
  "Extract dependencies for all detected project types in ROOT-DIR."
  (let ((deps '()))
    ;; JavaScript/TypeScript
    (when-let ((pkg-deps (gptelt--extract-package-json-deps
                          (expand-file-name "package.json" root-dir))))
      (push (cons :javascript pkg-deps) deps))

    ;; Clojure
    (when-let ((clj-deps (gptelt--extract-deps-edn
                          (expand-file-name "deps.edn" root-dir))))
      (push (cons :clojure clj-deps) deps))

    ;; Go
    (when-let ((go-deps (gptelt--extract-go-mod
                         (expand-file-name "go.mod" root-dir))))
      (push (cons :go go-deps) deps))

    ;; Python
    (when-let ((py-deps (gptelt--extract-requirements-txt
                         (expand-file-name "requirements.txt" root-dir))))
      (push (cons :python py-deps) deps))

    deps))

;;; Language Statistics

(defun gptelt--get-language-stats (root-dir)
  "Get language statistics using simple file extension counting."
  (let ((extensions '())
        (total-files 0))

    (cl-labels ((count-files (dir)
                  (let ((entries (ignore-errors
                                   (directory-files-and-attributes dir nil "^[^.]" t))))
                    (dolist (entry entries)
                      (let* ((name (car entry))
                             (is-dir (eq (cadr entry) t))
                             (full-path (expand-file-name name dir))
                             (rel-path (file-relative-name full-path root-dir)))
                        (unless (gptelt--should-exclude-path rel-path
                                                             '("\\.git/" "node_modules/" "\\.next/" "dist/" "build/"))
                          (if is-dir
                              (count-files full-path)
                            (when (string-match "\\.\\([a-zA-Z0-9]+\\)$" name)
                              (let ((ext (match-string 1 name)))
                                (cl-incf total-files)
                                (if (assoc ext extensions)
                                    (cl-incf (cdr (assoc ext extensions)))
                                  (push (cons ext 1) extensions)))))))))))

      (count-files root-dir))

    (let ((sorted-exts (cl-sort extensions (lambda (a b) (> (cdr a) (cdr b))))))
      (mapcar (lambda (ext-count)
                (list :extension (car ext-count)
                      :count (cdr ext-count)
                      :percentage (if (> total-files 0)
                                      (/ (* (cdr ext-count) 100.0) total-files)
                                    0)))
              (cl-subseq sorted-exts 0 (min 15 (length sorted-exts)))))))

;;; Main Workspace Analysis Functions

(defun gptelt-get-workspace-overview (&optional root-dir)
  "Get comprehensive workspace overview optimized for LLM consumption."
  (let* ((root (or root-dir (gptelt--get-project-root)))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (project-type (gptelt--detect-project-type root))
         (git-info (gptelt--get-git-info root))
         (lang-stats (gptelt--get-language-stats root))
         (hot-files (gptelt--get-hot-files root 20 6))
         (dependencies (gptelt--get-project-dependencies root)))

    (format "# Workspace Overview: %s

## Project Information
- **Name**: %s
- **Type**: %s
- **Root**: %s
- **Git**: %s
- **Branch**: %s
- **Last Commit**: %s

## Language Statistics (Top Extensions)
%s

## Hot Files (6 months, top 20)
%s

## Dependencies
%s

## File Structure (Abbreviated)
%s

---
*Generated at %s*
*Use specific file reading tools for detailed code inspection*"
            project-name
            project-name
            project-type
            root
            (if (plist-get git-info :is-git) "Yes" "No")
            (or (plist-get git-info :branch) "N/A")
            (or (plist-get git-info :last-commit) "N/A")

            ;; Language stats
            (if lang-stats
                (mapconcat (lambda (stat)
                             (format "- %s: %d files (%.1f%%)"
                                     (plist-get stat :extension)
                                     (plist-get stat :count)
                                     (plist-get stat :percentage)))
                           lang-stats "\n")
              "No language statistics available")

            ;; Hot files
            (if hot-files
                (mapconcat (lambda (file)
                             (format "- %s (%d changes)"
                                     (plist-get file :file)
                                     (plist-get file :changes)))
                           hot-files "\n")
              "No git history available")

            ;; Dependencies
            (if dependencies
                (mapconcat
                 (lambda (dep-group)
                   (let ((lang (car dep-group))
                         (info (cdr dep-group)))
                     (format "### %s\n%s"
                             (capitalize (symbol-name lang))
                             (if (plist-get info :dependencies)
                                 (concat "Dependencies:\n"
                                         (mapconcat (lambda (dep) (format "- %s" dep))
                                                    (cl-subseq (plist-get info :dependencies)
                                                               0 (min 20 (length (plist-get info :dependencies))))
                                                    "\n")
                                         (when (plist-get info :scripts)
                                           (concat "\n\nScripts:\n"
                                                   (mapconcat (lambda (script) (format "- %s" script))
                                                              (cl-subseq (plist-get info :scripts)
                                                                         0 (min 10 (length (plist-get info :scripts))))
                                                              "\n"))))
                               "No dependencies found"))))
                 dependencies "\n\n")
              "No dependency files found")

            ;; File tree (abbreviated)
            (let ((tree (gptelt--get-file-tree root 3 100)))
              (if tree
                  (mapconcat #'identity (cl-subseq tree 0 (min 50 (length tree))) "\n")
                "File tree unavailable"))

            (format-time-string "%Y-%m-%d %H:%M:%S"))))

(comment
  (gptelt-get-workspace-overview))

(defun gptelt-get-file-tree (&optional root-dir max-depth max-files)
  "Get file tree for workspace with customizable limits."
  (let* ((root (or root-dir (gptelt--get-project-root)))
         (tree (gptelt--get-file-tree root max-depth max-files)))
    (format "File tree for %s:\n\n%s"
            (file-name-nondirectory (directory-file-name root))
            (mapconcat #'identity tree "\n"))))

(comment
  (gptelt-get-file-tree nil 2))

(defun gptelt-get-project-summary (&optional root-dir)
  "Get concise project summary for token-efficient LLM consumption."
  (let* ((root (or root-dir (gptelt--get-project-root)))
         (project-name (file-name-nondirectory (directory-file-name root)))
         (project-type (gptelt--detect-project-type root))
         (git-info (gptelt--get-git-info root))
         (lang-stats (cl-subseq (gptelt--get-language-stats root) 0 5))
         (hot-files (cl-subseq (gptelt--get-hot-files root 10 3) 0 10)))

    (json-encode
     `((project . ,project-name)
       (type . ,project-type)
       (root . ,root)
       (git . ,(plist-get git-info :is-git))
       (branch . ,(plist-get git-info :branch))
       (languages . ,(mapcar (lambda (stat)
                               `(,(plist-get stat :extension) .
                                 ,(plist-get stat :count)))
                             lang-stats))
       (hot_files . ,(mapcar (lambda (file)
                               `(,(plist-get file :file) .
                                 ,(plist-get file :changes)))
                             hot-files))))))

(comment
  (gptelt-get-project-summary))

;;; Tool Registration

(when (fboundp 'gptelt-make-tool)
  (gptelt-make-tool
   :name "get_workspace_overview"
   :function #'gptelt-get-workspace-overview
   :description "Get comprehensive workspace/project overview optimized for LLM consumption. Includes project info, language stats, hot files, dependencies, and file structure."
   :args '((:name "root_dir" :type string :description "Optional root directory path (defaults to current project root)"))
   :category "workspace"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "get_file_tree"
   :function #'gptelt-get-file-tree
   :description "Get file tree for workspace with customizable depth and file limits."
   :args '((:name "root_dir" :type string :description "Optional root directory path")
           (:name "max_depth" :type integer :description "Maximum directory depth (default 4)")
           (:name "max_files" :type integer :description "Maximum number of files to include (default 200)"))
   :category "workspace"
   :confirm nil
   :include t)

  (gptelt-make-tool
   :name "get_project_summary"
   :function #'gptelt-get-project-summary
   :description "Get concise JSON project summary for token-efficient LLM consumption."
   :args '((:name "root_dir" :type string :description "Optional root directory path"))
   :category "workspace"
   :confirm nil
   :include t))

;;; workspace.el ends here
