;;; .nixpkgs/.doom.d/gptel-tools/ripgrep.el -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'rg)

;;; Utility functions
(defun gptelt-rg-tool--get-project-root ()
  "Get project root for current context."
  (if (fboundp '++workspace-current-project-root)
      (++workspace-current-project-root)
    (when (fboundp 'project-current)
      (when-let ((project (project-current)))
        (if (fboundp 'project-root)
            (project-root project)
          (car (project-roots project)))))))

(defun gptelt-rg-tool--resolve-directory (dir)
  "Resolve DIR to absolute path.
If DIR is relative, resolve it against the current project root."
  (cond
   ((string= dir "project") (or (gptelt-rg-tool--get-project-root) default-directory))
   ((string= dir "current") default-directory)
   ((file-name-absolute-p dir) dir)
   (t (let ((project-root (gptelt-rg-tool--get-project-root)))
        (if project-root
            (expand-file-name dir project-root)
          (expand-file-name dir default-directory))))))

;;; Main glob function
(defun gptelt-rg-tool-glob (pattern &optional path max-results)
  "Find files matching PATTERN in DIRECTORY using ripgrep.

PATTERN can be:
- A glob pattern like '*.el', '**/*.py', 'src/**/*.js'
- A ripgrep type alias like 'elisp', 'python', 'javascript'
- Multiple patterns separated by spaces

DIRECTORY is absolute or relative to project root. Defaults to project root.
MAX-RESULTS limits the number of results (default: 50).

Returns a list of file paths relative to the project root."
  (let* ((project-root (gptelt-rg-tool--get-project-root))
         (dir (gptelt-rg-tool--resolve-directory (or path project-root)))
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

(comment
  (gptelt-rg-tool-glob "**/*.clj"))

;;; Content search functions
(defun gptelt-rg-tool-search-content
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
  (let* ((project-root (gptelt-rg-tool--get-project-root))
         (dir (gptelt-rg-tool--resolve-directory (or path project-root)))
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

(defun gptelt-rg-tool-search-regex
    (pattern &optional path include max-results)
  "Search file contents using regular expressions."
  (gptelt-rg-tool-search-content pattern path include nil nil nil max-results))

;;; Tool registration

;; Register file finding tools
(gptelt-make-tool
 :name "glob"
 :function #'gptelt-rg-tool-glob
 :description
 "Fast file pattern matching tool that works with any codebase size using ripgrep.Efficiently finds files by name patterns and file types.

Supported pattern types:
- Glob patterns: '*.el', '**/*.py', 'src/**/*.{js,ts}'
- Ripgrep type aliases: 'elisp', 'python', 'javascript'
- Multiple patterns: '*.js *.ts' (space-separated)

Usage examples:
- Find all Python files: glob('*.py')
- Find TypeScript in src: glob('**/*.ts', 'src')
- Use type alias: glob('elisp')
- Limit results: glob('*.js', '/project', 20)

Path parameter supports:
- 'project' (default): Search from project root
- 'current': Search from current directory
- Absolute paths: '/specific/directory'
- Relative paths: Resolved against project root

Returns file paths relative to project root when possible, making them easy to use with other tools."
 :args '((:name "pattern"
          :type string
          :description "The glob pattern to match files against")
         (:name "path"
          :type string
          :optional t
          :description
          "The directory to search in. Defaults to the current project root.")
         (:name "max_results"
          :type integer
          :optional t
          :description "Maximum number of results to return (default: 50)"))
 :category "file-search"
 :confirm nil
 :include t)

(gptelt-make-tool
 :name "grep"
 :function #'gptelt-rg-tool-search-regex
 :description
 "Fast content search tool that works with any codebase size using ripgrep. Searches file contents using powerful regular expressions with high performance.

Regex capabilities:
- Full regex syntax: 'log.*Error', 'function\\s+\\w+', '^class \\w+'
- Case-sensitive and case-insensitive search
- Literal string matching (non-regex) option
- Context lines: Show N lines before/after matches

Usage examples:
- Find function definitions: grep('function\\s+\\w+')
- Search in specific files: grep('TODO', '/project', '*.py')
- Case-sensitive search: grep('Error', '/project', '*.log', true)
- With context: grep('import.*react', '/src', '*.js', false, false, 3)

File filtering options:
- include parameter: '*.js', '*.{ts,tsx}', '**/*.py'
- Searches hidden files by default
- Respects .gitignore patterns

Returns structured results with file paths (relative to project root), line numbers, column positions, and matched content. Perfect for code analysis and refactoring tasks."

 :args '((:name "pattern"
          :type string
          :description "The regular expression pattern to search for in file contents")
         (:name "path"
          :type string
          :optional t
          :description "The directory to search in. Defaults to the current project root.")
         (:name "include"
          :type string
          :optional t
          :description "File pattern to include in the search (e.g. '*.js', '*.{ts,tsx}')")
         (:name "max_results"
          :type integer
          :optional t
          :description "Maximum results (default: 50)"))
 :category "content-search"
 :confirm nil
 :include t)
