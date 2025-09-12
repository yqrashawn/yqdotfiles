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

(defun gptel-rg-tool-glob (pattern &optional directory max-results)
  "Find files matching PATTERN in DIRECTORY using ripgrep.

PATTERN can be:
- A glob pattern like '*.el', '**/*.py', 'src/**/*.js'
- A ripgrep type alias like 'elisp', 'python', 'javascript'
- Multiple patterns separated by spaces

DIRECTORY can be:
- 'project' for project root (default)
- 'current' for current directory
- An absolute or relative path

MAX-RESULTS limits the number of results returned (default: 50).

Returns a list of file paths matching the pattern."
  (let* ((dir (gptel-rg-tool--resolve-directory (or directory "project")))
         (limit (or max-results 50))
         (default-directory dir)
         (type-aliases (rg-get-type-aliases))
         results)

    (cond
     ((file-exists-p dir)
      (unless (file-directory-p dir)
        (error "Path is a file, not a directory: %s" dir)))
     ((not (file-directory-p dir))
      (error "Directory does not exist: %s" dir)))

    ;; Check if pattern is a type alias
    (if-let ((alias-entry (assoc pattern type-aliases)))
        (let* ((file-patterns (cdr alias-entry))
               (patterns-list (split-string file-patterns nil t))
               (rg-command (concat (rg-executable) " --files --hidden --type=" pattern)))
          (condition-case err
              (let ((output (shell-command-to-string rg-command)))
                (setq results (split-string output "\n" t)))
            (error (setq results nil))))

      ;; Handle as glob pattern(s)
      (let* ((patterns (split-string pattern nil t))
             (glob-args (mapconcat (lambda (p) (format "--glob='%s'" p)) patterns " "))
             (rg-command (format "%s --hidden --files %s" (rg-executable) glob-args)))
        (condition-case err
            (let ((output (shell-command-to-string rg-command)))
              (setq results (split-string output "\n" t)))
          (error (setq results nil)))))

    ;; Limit results and make paths relative to project root if possible
    (let* ((limited-results (seq-take results limit))
           (project-root (gptel-rg-tool--get-project-root)))
      (if project-root
          (mapcar (lambda (file)
                    (if (string-prefix-p project-root file)
                        (file-relative-name file project-root)
                      file))
                  limited-results)
        limited-results))))

(defun gptel-rg-tool-list-type-aliases ()
  "List all available ripgrep type aliases.
Returns an alist of (alias . file-patterns) pairs."
  (rg-get-type-aliases))

(defun gptel-rg-tool-get-file-types (file-path)
  "Get ripgrep type aliases that match FILE-PATH.
Returns a list of matching type aliases."
  (let ((filename (file-name-nondirectory file-path))
        (type-aliases (rg-get-type-aliases))
        matching-types)
    (dolist (alias type-aliases)
      (let ((patterns (split-string (cdr alias) nil t)))
        (when (cl-some (lambda (pattern)
                         (string-match (wildcard-to-regexp pattern) filename))
                       patterns)
          (push (car alias) matching-types))))
    matching-types))

;;; Content search functions

(defun gptel-rg-tool-search-content
    (pattern &optional directory file-types case-sensitive literal
             context-lines max-results)
  "Search for PATTERN in file contents using ripgrep.

PATTERN is the text to search for.
DIRECTORY can be 'project' (default), 'current', or a path.
FILE-TYPES can be a ripgrep type alias or glob pattern.
CASE-SENSITIVE determines case sensitivity (default: smart case).
LITERAL treats PATTERN as literal text instead of regex.
CONTEXT-LINES shows N lines before and after matches.
MAX-RESULTS limits the number of results (default: 50).

Returns a list of search results with file paths, line numbers, and content."
  (let* ((dir (gptel-rg-tool--resolve-directory (or directory "project")))
         (limit (or max-results 50))
         (context (or context-lines 0))
         (default-directory dir)
         (rg-cmd (rg-executable))
         (args (list)))

    (cond
     ((file-exists-p dir)
      (unless (file-directory-p dir)
        (error "Path is a file, not a directory: %s" dir)))
     ((not (file-directory-p dir))
      (error "Directory does not exist: %s" dir)))

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

    (when file-types
      (if (assoc file-types (rg-get-type-aliases))
          (push (format "--type=%s" file-types) args)
        (push (format "--glob='%s'" file-types) args)))

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
            (push (list :file file :line line-num :column column :content content) results))))

      (reverse results))))

(defun gptel-rg-tool-search-literal (text &optional directory file-types max-results)
  "Search for literal TEXT in file contents.
This is a simplified version of content search for literal strings."
  (gptel-rg-tool-search-content text directory file-types nil t nil max-results))

(defun gptel-rg-tool-search-regex (pattern &optional directory file-types max-results)
  "Search for regex PATTERN in file contents.
This is a simplified version of content search for regular expressions."
  (gptel-rg-tool-search-content pattern directory file-types nil nil nil max-results))

;;; Tool registration

;; Register file finding tools
(when (fboundp 'gptel-make-tool)
  (gptel-make-tool
   :name "glob"
   :function #'gptel-rg-tool-glob
   :description "Find files matching glob patterns or ripgrep type aliases using ripgrep. Supports patterns like '*.el', '**/*.py', 'src/**/*.js' or type aliases like 'elisp', 'python', 'javascript'."
   :args (list '(:name "pattern" :type string
                 :description "Glob pattern (*.el, **/*.py) or ripgrep type alias (elisp, python, javascript)")
               '(:name "directory" :type string :optional t
                 :description "Directory to search: 'project' (default), or absolute/relative path")
               '(:name "max-results" :type integer :optional t
                 :description "Maximum number of results to return (default: 50)"))
   :category "file-search"
   :confirm nil
   :include t)

  ;; Register helper tool for listing type aliases
  (gptel-make-tool
   :name "list-ripgrep-types"
   :function #'gptel-rg-tool-list-type-aliases
   :description "List all available ripgrep type aliases and their corresponding file patterns."
   :args nil
   :category "file-search"
   :confirm nil
   :include t)

  ;; Register helper tool for getting file types
  (gptel-make-tool
   :name "get-file-types"
   :function #'gptel-rg-tool-get-file-types
   :description "Get ripgrep type aliases that match a given file path."
   :args (list '(:name "file-path" :type string
                 :description "File path to check for matching type aliases"))
   :category "file-search"
   :confirm nil
   :include t)

  ;; Register content search tools
  (gptel-make-tool
   :name "search-content"
   :function #'gptel-rg-tool-search-content
   :description "Search for patterns in file contents using ripgrep. Supports regex patterns, literal text, case sensitivity, file type filtering, and context lines."
   :args (list '(:name "pattern" :type string
                 :description "Text or regex pattern to search for")
               '(:name "directory" :type string :optional t
                 :description "Directory to search: 'project' (this is the default, it's the project that user is working on), or path (user may provide other files that's not in the current project)")
               '(:name "file-types" :type string :optional t
                 :description "Ripgrep type alias (e.g., 'python', 'js') or glob pattern")
               '(:name "case-sensitive" :type boolean :optional t
                 :description "Force case-sensitive search (default: smart case)")
               '(:name "literal" :type boolean :optional t
                 :description "Treat pattern as literal text instead of regex")
               '(:name "context-lines" :type integer :optional t
                 :description "Number of context lines to show around matches")
               '(:name "max-results" :type integer :optional t
                 :description "Maximum number of results (default: 50)"))
   :category "content-search"
   :confirm nil
   :include t)

  (gptel-make-tool
   :name "search-literal"
   :function #'gptel-rg-tool-search-literal
   :description "Search for literal text in file contents (no regex interpretation)."
   :args (list '(:name "text" :type string
                 :description "Literal text to search for")
               '(:name "directory" :type string :optional t
                 :description "Directory to search: 'project' (this is the default, it's the project that user is working on), or path (user may provide other files that's not in the current project)")
               '(:name "file-types" :type string :optional t
                 :description "File type filter (ripgrep alias or glob)")
               '(:name "max-results" :type integer :optional t
                 :description "Maximum results (default: 50)"))
   :category "content-search"
   :confirm nil
   :include t)

  (gptel-make-tool
   :name "search-regex"
   :function #'gptel-rg-tool-search-regex
   :description "Search for regex patterns in file contents."
   :args (list '(:name "pattern" :type string
                 :description "Regular expression pattern to search for")
               '(:name "directory" :type string :optional t
                 :description "Directory to search: 'project' (this is the default, it's the project that user is working on), or path (user may provide other files that's not in the current project)")
               '(:name "file-types" :type string :optional t
                 :description "File type filter (ripgrep alias or glob)")
               '(:name "max-results" :type integer :optional t
                 :description "Maximum results (default: 50)"))
   :category "content-search"
   :confirm nil
   :include t))

(provide 'gptel-rg-tool)

;;; glob tool using rg.el
;; glob in given dir for given args, if no dir given, use project root from ++workspace-current-project-root
