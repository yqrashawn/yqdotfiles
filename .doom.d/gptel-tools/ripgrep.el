;;; .nixpkgs/.doom.d/gptel-tools/ripgrep.el -*- lexical-binding: t; -*-

;;; Commentary:

;; Glob tool using rg.el for file pattern matching and type alias operations.
;; Provides functionality to search for files matching glob patterns using ripgrep's
;; built-in type system and custom patterns.

;;; Code:

(require 'rg)

;;; Utility functions
(defun gptel-rg-tool--get-project-root ()
  "Get project root for current context."
  (if (fboundp '++workspace-current-project-root)
      (++workspace-current-project-root)
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (if (fboundp 'project-root)
            (project-root project)
          (car (project-roots project)))))))

(defun gptel-rg-tool--resolve-directory (dir)
  "Resolve DIR to absolute path.
If DIR is relative, resolve it against the current project root."
  (cond
   ((string= dir "project") (or (gptel-rg-tool--get-project-root) default-directory))
   ((string= dir "current") default-directory)
   ((file-name-absolute-p dir) dir)
   (t (let ((project-root (gptel-rg-tool--get-project-root)))
        (if project-root
            (expand-file-name dir project-root)
          (expand-file-name dir default-directory))))))

;;; Main glob function
(defun gptel-rg-tool-glob (pattern &optional path max-results)
  "Find files matching PATTERN in DIRECTORY using ripgrep.

PATTERN can be:
- A glob pattern like '*.el', '**/*.py', 'src/**/*.js'
- A ripgrep type alias like 'elisp', 'python', 'javascript'
- Multiple patterns separated by spaces

DIRECTORY is absolute or relative to project root. Defaults to project root.
MAX-RESULTS limits the number of results (default: 50).

Returns a list of file paths relative to the project root."
  (let* ((project-root (gptel-rg-tool--get-project-root))
         (dir (gptel-rg-tool--resolve-directory (or path project-root)))
         (limit (or max-results 50))
         (default-directory dir)
         (type-aliases (rg-get-type-aliases))
         results)
    (unless (and dir (file-directory-p dir))
      (error "Directory does not exist: %s" dir))
    ;; Check if pattern is a type alias
    (if-let ((alias-entry (assoc pattern type-aliases)))
        (let* ((rg-command (concat (rg-executable) " --files --hidden --type=" pattern)))
          (condition-case _
              (let ((output (shell-command-to-string rg-command)))
                (setq results (split-string output "\n" t)))
            (error (setq results nil))))
      ;; Handle as glob pattern(s)
      (let* ((patterns (split-string pattern nil t))
             (glob-args (mapconcat (lambda (p) (format "--glob='%s'" p)) patterns " "))
             (rg-command (format "%s --hidden --files %s" (rg-executable) glob-args)))
        (condition-case _
            (let ((output (shell-command-to-string rg-command)))
              (setq results (split-string output "\n" t)))
          (error (setq results nil)))))
    (let ((limited-results (seq-take results limit)))
      (if project-root
          (mapcar (lambda (file)
                    (if (string-prefix-p project-root file)
                        (file-relative-name file project-root)
                      file))
                  limited-results)
        limited-results))))

;;; Content search functions
(defun gptel-rg-tool-search-content
    (pattern &optional path include case-sensitive literal context-lines max-results)
  "Search for PATTERN in file contents using ripgrep.

PATTERN is the text to search for.
DIRECTORY is absolute or relative to project root. Defaults to project root.
FILE-TYPES can be a ripgrep type alias or glob pattern.
CASE-SENSITIVE determines case sensitivity (default: smart case).
LITERAL treats PATTERN as literal text instead of regex.
CONTEXT-LINES shows N lines before and after matches.
MAX-RESULTS limits the number of results (default: 50).

Returns a list of search results with file paths, line numbers, and content."
  (let* ((project-root (gptel-rg-tool--get-project-root))
         (dir (gptel-rg-tool--resolve-directory (or path project-root)))
         (limit (or max-results 50))
         (context (or context-lines 0))
         (default-directory dir)
         (rg-cmd (rg-executable))
         (args (list)))
    (unless (and dir (file-directory-p dir))
      (error "Directory does not exist: %s" dir))
    ;; Build ripgrep arguments
    (push "--hidden" args)
    (push "--color=never" args)
    (push "--line-number" args)
    (push "--column" args)
    (push "--no-heading" args)
    (push (format "--max-count=%d" limit) args)
    (when context-lines
      (push (format "--context=%d" context-lines) args))
    (when literal
      (push "--fixed-strings" args))
    (when case-sensitive
      (push "--case-sensitive" args))
    (when include
      (push (format "--glob='%s'" include) args))
    (push "--" args)
    (push (format "'%s'" pattern) args)
    (push "." args)
    (let* ((command (mapconcat 'identity (cons rg-cmd (reverse args)) " "))
           (output (shell-command-to-string command))
           (lines (split-string output "\n" t))
           results)
      (dolist (line lines)
        (when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\([0-9]+\\):\\(.*\\)$" line)
          (let ((file (match-string 1 line))
                (line-num (string-to-number (match-string 2 line)))
                (column (string-to-number (match-string 3 line)))
                (content (match-string 4 line)))
            (push (list :file (if (and project-root (string-prefix-p project-root file))
                                  (file-relative-name file project-root)
                                file)
                        :line line-num :column column :content content) results))))
      (reverse results))))

(defun gptel-rg-tool-search-regex
    (pattern &optional path include max-results)
  "Search file contents using regular expressions."
  (gptel-rg-tool-search-content pattern path include nil nil nil max-results))

;;; Tool registration

;; Register file finding tools
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "glob"
   :function #'gptel-rg-tool-glob
   :description "- Fast file pattern matching tool that works with any codebase size
- Supports glob patterns like \"**/*.clj\" or \"src/**/*.{clj,cljs,cljc}\"
- Returns matching file paths sorted by modification time
- Use this tool when you need to find files by name patterns"
   :args (list '(:name "pattern" :type string
                 :description "The glob pattern to match files against")
               '(:name "path" :type string :optional t
                 :description "The directory to search in. Defaults to the current project root.")
               '(:name "max_results" :type integer :optional t
                 :description "Maximum number of results to return (default: 50)"))
   :category "file-search"
   :confirm nil
   :include t)

  (gptel-make-tool
   :name "grep"
   :function #'gptel-rg-tool-search-regex
   :description "
- Fast content search tool that works with any codebase size
- Searches file contents using regular expressions
- Supports full regex syntax (eg. \"log.*Error\", \"function\\s+\\w+\", etc.)
- Filter files by pattern with the include parameter (eg. \"*.js\", \"*.{ts,tsx}\")
- Returns matching file paths sorted by modification time
- Use this tool when you need to find files containing specific patterns "
   :args (list '(:name "pattern" :type string
                 :description "The regular expression pattern to search for in file contents")
               '(:name "path" :type string :optional t
                 :description "The directory to search in. Defaults to the current project root.")
               '(:name "include" :type string :optional t
                 :description "File pattern to include in the search (e.g. '*.js', '*.{ts,tsx}')")
               '(:name "max_results" :type integer :optional t
                 :description "Maximum results (default: 50)"))
   :category "content-search"
   :confirm nil
   :include t))

(provide 'gptel-rg-tool)

;;; glob tool using rg.el
;; glob in given dir for given args, if no dir given, use project root from ++workspace-current-project-root
