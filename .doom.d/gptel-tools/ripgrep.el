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
             ;; Normalize patterns: prepend ** for relative dir patterns to search recursively
             (normalized-patterns
              (mapcar (lambda (p)
                        ;; If pattern has / but doesn't start with /, ., or **, prepend **/
                        (if (and (not (string-prefix-p "/" p))
                                 (not (string-prefix-p "." p))
                                 (not (string-prefix-p "**" p))
                                 (string-match-p "/" p))
                            (concat "**/" p)
                          p))
                      patterns))
             (glob-args (mapconcat (lambda (p) (format "--glob='%s'" p)) normalized-patterns " "))
             (rg-command (format "%s --glob-case-insensitive --hidden --files %s" (rg-executable) glob-args)))
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
  (gptelt-rg-tool-glob "**/*.clj")
  (gptelt-rg-tool-glob "gptel-tools/*.el"))

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
    (push "-g" args)
    (push "'!/.git/'" args)
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

(comment
  (gptelt-rg-tool-search-content "defun" nil "*.el"))

(defun gptelt-rg-tool-search-regex
    (pattern &optional path include max-results)
  "Search file contents using regular expressions."
  (gptelt-rg-tool-search-content pattern path include nil nil nil max-results))

(comment
  (gptelt-rg-tool-search-regex "TODO|regex")
  (gptelt-rg-tool-search-regex "defun.*gptelt" nil "*.el")
  (gptelt-rg-tool-search-regex "src/stores/global"
                               "/Users/yqrashawn/workspace/office/perpdex/perpdex-nextjs"
                               nil 20))

;;; Tool registration

;; Register file finding tools
(gptelt-make-tool
 :name "glob"
 :function #'gptelt-rg-tool-glob
 :description
 "Find files by pattern using ripgrep. Fast and works with any codebase size.

PATTERN TYPES:
1. Extensions: '*.py', '*.{js,ts,jsx,tsx}'
2. Name patterns: 'test-*.clj', '*-config.json'
3. Recursive: '**/*.py' (all subdirectories), 'src/**/*.ts' (under src/)
4. Relative paths: 'gptel-tools/*.el', 'lib/utils/*.js' (relative to search directory)
5. Absolute paths: '.doom.d/foo/*.el', '/absolute/path/*.py'
6. Type aliases: 'elisp', 'python', 'javascript', 'typescript', 'rust', 'go'
7. Multiple: '*.js *.json' (space-separated)

EXAMPLES:
glob('*.py')                        → Python files at root level
glob('**/*.test.js')                → All test files recursively
glob('gptel-tools/*.el')            → Files in gptel-tools/ directory (relative)
glob('.doom.d/**/*-test.el')        → Test files in .doom.d/ tree
glob('src/**/*.{ts,tsx}')           → TypeScript files under src/
glob('elisp')                       → All Elisp files (type alias)
glob('*-config.json', 'src')        → Config files in src/ directory
glob('*.el', '.doom.d/gptel-tools') → Elisp in specific directory

PATH PARAMETER (search root):
- Omit/'project': Project root (default)
- 'current': Current directory
- Absolute: '/specific/directory'
- Relative: 'src', 'lib' (resolved from project root)

PERFORMANCE:
- Respects .gitignore automatically
- Searches hidden files/directories
- Returns max 50 results by default (use max_results to change)
- Extremely fast even on large codebases"
 :args '((:name "pattern"
          :type string
          :description "Glob pattern, type alias, or space-separated patterns to match files")
         (:name "path"
          :type string
          :optional t
          :description "Directory to search in. Defaults to project root. Can be 'project', 'current', absolute, or relative path.")
         (:name "max_results"
          :type integer
          :optional t
          :description "Maximum number of results to return (default: 50)"))
 :category "file"
 :confirm nil
 :include t)

(gptelt-make-tool
 :name "grep"
 :function #'gptelt-rg-tool-search-regex
 :description
 "Search file contents using regular expressions with ripgrep. Fast and works with any codebase size.

REGEX PATTERNS:
- Wildcards: 'defun.*gptelt' (any defun containing gptelt)
- Word boundaries: '\\bfunction\\s+\\w+' (function followed by identifier)
- Line anchors: '^class \\w+' (start), '\\.js$' (end of line)
- Alternation: 'TODO|FIXME|HACK' (multiple keywords)
- Character classes: '[A-Z][a-z]+Error' (CamelCase), '[^;]' (negated)
- Quantifiers: '\\d+' (digits), '\\d{3,}' (3+ digits), '[A-Z]{2,}' (2+ uppercase)
- Whitespace: '\\s+' (one or more spaces), '\\s*' (zero or more)
- Case-insensitive: '(?i)todo|fixme' (ignore case flag)

EXAMPLES:
grep('TODO')                          → Find all TODO comments
grep('defun gptelt', nil, '*.el')     → Functions in Elisp files
grep('TODO|FIXME', nil, '*.{js,ts}')  → Multiple patterns in JS/TS
grep('function\\s+\\w+')              → Function definitions (regex)
grep('import.*react', nil, '*.jsx')   → React imports
grep('\\bdefun\\b')                   → Word boundary (exact 'defun')
grep('^\\s*;;')                       → Lines starting with comments
grep('(?i)error|warning')             → Case-insensitive search

FILTERING:
- include: '*.js', '*.{ts,tsx}', '*-test.el'
- path: Directory to search (default: project root)
- Respects .gitignore automatically
- Searches hidden files

RETURNS:
List of matches with :file (relative path), :line, :column, :content
Limited to 50 results by default (use max_results to change)"

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
 :category "file"
 :confirm nil
 :include t)
