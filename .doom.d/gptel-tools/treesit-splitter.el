;;; .nixpkgs/.doom.d/gptel-tools/treesit-splitter.el -*- lexical-binding: t;coding: utf-8-unix; -*-

(require 'treesit)
(require 'lsp-mode nil t)
(require 'cl-lib)

(defgroup treesit-splitter nil
  "Tree-sitter based code splitter for RAG applications."
  :group 'tools
  :prefix "treesit-splitter-")

(defcustom treesit-splitter-chunk-size 2500
  "Default chunk size in characters."
  :type 'integer
  :group 'treesit-splitter)

(defcustom treesit-splitter-chunk-overlap 300
  "Default chunk overlap in characters."
  :type 'integer
  :group 'treesit-splitter)

(defcustom treesit-splitter-min-chunk-size 100
  "Minimum chunk size in characters."
  :type 'integer
  :group 'treesit-splitter)

(defvar treesit-splitter-splittable-node-types
  '((javascript . (function_declaration arrow_function method_definition export_statement class_declaration))
    (typescript . (function_declaration arrow_function method_definition export_statement interface_declaration type_alias_declaration class_declaration))
    (python . (function_definition async_function_definition decorated_definition class_definition))
    (java . (method_declaration constructor_declaration class_declaration interface_declaration))
    (cpp . (function_definition declaration class_specifier namespace_definition))
    (c . (function_definition struct_specifier))
    (go . (function_declaration method_declaration type_declaration var_declaration const_declaration))
    (rust . (function_item impl_item struct_item enum_item trait_item mod_item))
    (csharp . (method_declaration class_declaration interface_declaration struct_declaration enum_declaration))
    (scala . (method_declaration class_declaration interface_declaration constructor_declaration))
    (clojure . (list_lit)))
  "Node types that represent logical code units for each language.
Ordered by preference - smaller, more specific nodes first.")

(defvar treesit-splitter-language-map
  '((js . javascript)
    (ts . typescript)
    (py . python)
    (c++ . cpp)
    (cs . csharp)
    (rs . rust)
    (clj . clojure)
    (cljc . clojure)
    (cljs . clojure))
  "Mapping from file extensions to normalized language names.")

(cl-defstruct treesit-splitter-chunk
  content
  start-line
  end-line
  language
  file-path
  node-type
  ;; Enhanced LSP metadata
  definition-name      ; Function/class name
  definition-type      ; function, class, method, etc.
  signature           ; Function signature from LSP
  docstring          ; Documentation string
  hover-info         ; LSP hover information
  symbols-defined    ; Symbols defined in chunk
  symbols-used       ; External symbols referenced
  dependencies       ; List of imports/requires (extracted)
  complexity-score   ; Rough complexity estimate
  checksum           ; Content hash for deduplication
  related-chunks)    ; Semantically related chunks

(defun treesit-splitter--extract-dependencies (content language)
  "Extract dependencies/imports from CONTENT for LANGUAGE."
  (pcase language
    ('clojure
     ;; Clojure: (ns ...) and (require ...)
     (let ((deps '()))
       (let ((pos 0))
         (while (string-match "(require\s-+'?\([a-zA-Z0-9_.\-]+\)" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (let ((pos 0))
         (while (string-match "(ns\s-+\([^\s)]+\)" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (delete-dups deps)))
    ((or 'javascript 'typescript)
     ;; JavaScript/TypeScript: import ... from ... / require(...)
     (let ((deps '()))
       (let ((pos 0))
         (while (string-match "import.*from[ \t]*['\"]\([^'\"]+\)['\"]" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (let ((pos 0))
         (while (string-match "require([ \t]*['\"]\([^'\"]+\)['\"]" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (let ((pos 0))
         (while (string-match "export.*from[ \t]*['\"]\([^'\"]+\)['\"]" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (delete-dups deps)))
    ('python
     ;; Python: import, from ... import
     (let ((deps '()))
       (let ((pos 0))
         (while (string-match "^import\s-+\([a-zA-Z0-9_.]+\)" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (let ((pos 0))
         (while (string-match "^from\s-+\([a-zA-Z0-9_.]+\)\s-+import" content pos)
           (push (match-string 1 content) deps)
           (setq pos (match-end 0))))
       (delete-dups deps)))
    (_ nil)))

(defun treesit-splitter--enhance-chunk-with-lsp (chunk pos &optional original-buffer)
  "Enhance CHUNK with LSP metadata at POS using ORIGINAL-BUFFER for LSP context."
  (let* ((content (treesit-splitter-chunk-content chunk))
         (node-type (treesit-splitter-chunk-node-type chunk))
         (language (treesit-splitter-chunk-language chunk))
         ;; Extract basic information
         (def-name (treesit-splitter--extract-function-name content node-type))
         (symbols-used (treesit-splitter--extract-symbols-used content language))
         (complexity (treesit-splitter--calculate-complexity content))
         (checksum (secure-hash 'md5 content))
         (dependencies (treesit-splitter--extract-dependencies content language))
         ;; Get LSP information from original buffer
         (hover-info (treesit-splitter--get-lsp-hover-info pos original-buffer))
         (symbol-info (treesit-splitter--get-lsp-symbol-info pos original-buffer)))

    ;; Set enhanced metadata
    (setf (treesit-splitter-chunk-definition-name chunk) def-name)
    (setf (treesit-splitter-chunk-definition-type chunk)
          (treesit-splitter--normalize-definition-type node-type))
    (setf (treesit-splitter-chunk-hover-info chunk) hover-info)
    (setf (treesit-splitter-chunk-symbols-used chunk) symbols-used)
    (setf (treesit-splitter-chunk-complexity-score chunk) complexity)
    (setf (treesit-splitter-chunk-checksum chunk) checksum)
    (setf (treesit-splitter-chunk-dependencies chunk) dependencies)

    ;; Extract signature and docstring from LSP if available
    (when symbol-info
      (setf (treesit-splitter-chunk-signature chunk)
            (lsp:symbol-information-name symbol-info)))

    ;; Extract docstring from content
    (setf (treesit-splitter-chunk-docstring chunk)
          (treesit-splitter--extract-docstring content language))))


(defun treesit-splitter--normalize-language (language)
  "Normalize LANGUAGE name to canonical form."
  (let ((lang-symbol (if (stringp language)
                         (intern language)
                       language)))
    (or (alist-get lang-symbol treesit-splitter-language-map)
        lang-symbol)))

(defun treesit-splitter--get-splittable-types (language)
  "Get splittable node types for LANGUAGE."
  (let ((normalized-lang (treesit-splitter--normalize-language language)))
    (alist-get normalized-lang treesit-splitter-splittable-node-types)))

(defun treesit-splitter--language-supported-p (language)
  "Check if LANGUAGE is supported by tree-sitter splitter."
  (let ((normalized-lang (treesit-splitter--normalize-language language)))
    (and (assoc normalized-lang treesit-splitter-splittable-node-types)
         (treesit-language-available-p normalized-lang))))

(defun treesit-splitter--node-text (node)
  "Get text content of NODE."
  (buffer-substring-no-properties
   (treesit-node-start node)
   (treesit-node-end node)))

(defun treesit-splitter--node-line-range (node)
  "Get line range (start . end) of NODE."
  (let ((start-pos (treesit-node-start node))
        (end-pos (treesit-node-end node)))
    (cons (line-number-at-pos start-pos)
          (line-number-at-pos end-pos))))

(defun treesit-splitter--clojure-definition-p (content)
  "Check if CONTENT is a Clojure definition form."
  (not (null (string-match-p "^\\s-*(\\(defn\\|defmacro\\|defprotocol\\|defrecord\\|deftype\\|ns\\|def\\)\\s-" content))))

(defun treesit-splitter--extract-clojure-definition-type (content node-type language)
  "Extract the actual definition type from Clojure CONTENT, or return NODE-TYPE."
  (if (eq language 'clojure) ; Only for Clojure
      (cond
       ((string-match "^\\s-*(defn\\s-" content) "defn")
       ((string-match "^\\s-*(defmacro\\s-" content) "defmacro")
       ((string-match "^\\s-*(defprotocol\\s-" content) "defprotocol")
       ((string-match "^\\s-*(defrecord\\s-" content) "defrecord")
       ((string-match "^\\s-*(deftype\\s-" content) "deftype")
       ((string-match "^\\s-*(ns\\s-" content) "ns")
       ((string-match "^\\s-*(def\\s-" content) "def")
       (t node-type))
    node-type))

(defun treesit-splitter--extract-ast-chunks (root-node splittable-types language file-path &optional original-buffer)
  "Extract chunks from ROOT-NODE based on SPLITTABLE-TYPES.
ORIGINAL-BUFFER is the source buffer for LSP context."
  (let ((chunks '())
        (processed-ranges '()))

    ;; Helper function to check if a range overlaps with already processed ranges
    (cl-flet ((range-overlaps-p (start end)
                (cl-some (lambda (range)
                           (let ((range-start (car range))
                                 (range-end (cdr range)))
                             (not (or (>= start range-end) (<= end range-start)))))
                         processed-ranges)))

      (treesit-search-subtree
       root-node
       (lambda (node)
         (when (memq (intern (treesit-node-type node)) splittable-types)
           (let* ((start-pos (treesit-node-start node))
                  (end-pos (treesit-node-end node))
                  (line-range (treesit-splitter--node-line-range node))
                  (start-line (car line-range))
                  (end-line (cdr line-range))
                  (content (treesit-splitter--node-text node))
                  (node-type (treesit-node-type node)))

             ;; Skip if this range overlaps with already processed ranges
             (unless (range-overlaps-p start-line end-line)
               ;; Special handling for Clojure list_lit nodes
               (when (and (eq language 'clojure) (string= node-type "list_lit"))
                 ;; Only include list_lit that starts with Clojure definition forms
                 (unless (treesit-splitter--clojure-definition-p content)
                   (setq content nil))) ; Skip this node

               (when (and content (> (length (string-trim content)) treesit-splitter-min-chunk-size))
                 (let ((chunk (make-treesit-splitter-chunk
                               :content content
                               :start-line start-line
                               :end-line end-line
                               :language language
                               :file-path file-path
                               :node-type (treesit-splitter--extract-clojure-definition-type content node-type language))))
                   ;; Enhance with LSP metadata if available
                   (treesit-splitter--enhance-chunk-with-lsp chunk start-pos original-buffer)
                   (push chunk chunks)
                   ;; Mark this range as processed
                   (push (cons start-line end-line) processed-ranges)))))
           nil) ; Continue searching
         nil)
       t)
      (nreverse chunks))))

(defun treesit-splitter--split-large-chunk (chunk)
  "Split a large CHUNK into smaller chunks with semantic awareness."
  (let* ((content (treesit-splitter-chunk-content chunk))
         (language (treesit-splitter-chunk-language chunk))
         (node-type (treesit-splitter-chunk-node-type chunk)))

    ;; For large class/module chunks, try to split by methods/functions within them
    (if (member node-type '("class_declaration" "class_specifier" "namespace_definition" "mod_item"))
        (treesit-splitter--split-large-chunk-semantically chunk)
      ;; For other large chunks, use simple line-based splitting but mark as fragments
      (treesit-splitter--split-large-chunk-by-lines chunk))))

(defun treesit-splitter--split-large-chunk-semantically (chunk)
  "Split a large class/namespace CHUNK by finding internal methods/functions."
  (let* ((content (treesit-splitter-chunk-content chunk))
         (language (treesit-splitter-chunk-language chunk))
         (sub-chunks '()))

    ;; Try to parse the chunk content to find internal structures
    (condition-case nil
        (with-temp-buffer
          (insert content)
          (let* ((parser (treesit-parser-create language))
                 (root-node (treesit-parser-root-node parser))
                 (internal-types (pcase language
                                   ((or 'javascript 'typescript) '(method_definition function_declaration arrow_function))
                                   ('python '(function_definition async_function_definition))
                                   ('java '(method_declaration constructor_declaration))
                                   (_ '(method_definition function_definition))))
                 (found-internals '()))

            ;; Find internal methods/functions
            (treesit-search-subtree
             root-node
             (lambda (node)
               (when (memq (intern (treesit-node-type node)) internal-types)
                 (let* ((start-pos (treesit-node-start node))
                        (end-pos (treesit-node-end node))
                        (line-start (line-number-at-pos start-pos))
                        (line-end (line-number-at-pos end-pos))
                        (node-content (buffer-substring start-pos end-pos)))
                   (when (> (length (string-trim node-content)) treesit-splitter-min-chunk-size)
                     (push (make-treesit-splitter-chunk
                            :content node-content
                            :start-line (+ (treesit-splitter-chunk-start-line chunk) line-start -1)
                            :end-line (+ (treesit-splitter-chunk-start-line chunk) line-end -1)
                            :language language
                            :file-path (treesit-splitter-chunk-file-path chunk)
                            :node-type (treesit-node-type node))
                           found-internals))))
               nil)
             t)

            (if found-internals
                (nreverse found-internals)
              ;; Fallback to line-based splitting if no internal structures found
              (treesit-splitter--split-large-chunk-by-lines chunk))))
      (error
       ;; If tree-sitter parsing fails, fallback to line-based splitting
       (treesit-splitter--split-large-chunk-by-lines chunk)))))

(defun treesit-splitter--split-large-chunk-by-lines (chunk)
  "Split a large CHUNK by lines, preserving metadata for the first sub-chunk."
  (let* ((content (treesit-splitter-chunk-content chunk))
         (lines (split-string content "\n" t))
         (current-chunk "")
         (current-start-line (treesit-splitter-chunk-start-line chunk))
         (current-line-count 0)
         (sub-chunks '())
         (first-chunk-p t))

    (dolist (line lines)
      (let ((line-with-newline (concat line "\n")))
        (if (and (> (length current-chunk) 0)
                 (> (+ (length current-chunk) (length line-with-newline))
                    treesit-splitter-chunk-size))
            ;; Create a sub-chunk
            (progn
              (let ((sub-chunk (make-treesit-splitter-chunk
                                :content (string-trim current-chunk)
                                :start-line current-start-line
                                :end-line (+ current-start-line current-line-count -1)
                                :language (treesit-splitter-chunk-language chunk)
                                :file-path (treesit-splitter-chunk-file-path chunk)
                                :node-type (if first-chunk-p
                                               (treesit-splitter-chunk-node-type chunk)
                                             (concat (treesit-splitter-chunk-node-type chunk) "-fragment")))))
                ;; Preserve metadata for the first sub-chunk
                (when first-chunk-p
                  (setf (treesit-splitter-chunk-definition-name sub-chunk)
                        (treesit-splitter-chunk-definition-name chunk))
                  (setf (treesit-splitter-chunk-definition-type sub-chunk)
                        (treesit-splitter-chunk-definition-type chunk))
                  (setf (treesit-splitter-chunk-signature sub-chunk)
                        (treesit-splitter-chunk-signature chunk))
                  (setf (treesit-splitter-chunk-docstring sub-chunk)
                        (treesit-splitter-chunk-docstring chunk))
                  (setf (treesit-splitter-chunk-hover-info sub-chunk)
                        (treesit-splitter-chunk-hover-info chunk))
                  (setf (treesit-splitter-chunk-symbols-used sub-chunk)
                        (treesit-splitter-chunk-symbols-used chunk))
                  (setf (treesit-splitter-chunk-complexity-score sub-chunk)
                        (treesit-splitter-chunk-complexity-score chunk))
                  (setq first-chunk-p nil))
                (push sub-chunk sub-chunks))
              (setq current-chunk line-with-newline
                    current-start-line (+ current-start-line current-line-count)
                    current-line-count 1))
          ;; Add line to current chunk
          (setq current-chunk (concat current-chunk line-with-newline)
                current-line-count (1+ current-line-count)))))

    ;; Add the last sub-chunk
    (when (> (length (string-trim current-chunk)) 0)
      (let ((sub-chunk (make-treesit-splitter-chunk
                        :content (string-trim current-chunk)
                        :start-line current-start-line
                        :end-line (+ current-start-line current-line-count -1)
                        :language (treesit-splitter-chunk-language chunk)
                        :file-path (treesit-splitter-chunk-file-path chunk)
                        :node-type (if first-chunk-p
                                       (treesit-splitter-chunk-node-type chunk)
                                     (concat (treesit-splitter-chunk-node-type chunk) "-fragment")))))
        ;; Preserve metadata for the first sub-chunk (if this is the only chunk)
        (when first-chunk-p
          (setf (treesit-splitter-chunk-definition-name sub-chunk)
                (treesit-splitter-chunk-definition-name chunk))
          (setf (treesit-splitter-chunk-definition-type sub-chunk)
                (treesit-splitter-chunk-definition-type chunk))
          (setf (treesit-splitter-chunk-signature sub-chunk)
                (treesit-splitter-chunk-signature chunk))
          (setf (treesit-splitter-chunk-docstring sub-chunk)
                (treesit-splitter-chunk-docstring chunk))
          (setf (treesit-splitter-chunk-hover-info sub-chunk)
                (treesit-splitter-chunk-hover-info chunk))
          (setf (treesit-splitter-chunk-symbols-used sub-chunk)
                (treesit-splitter-chunk-symbols-used chunk))
          (setf (treesit-splitter-chunk-complexity-score sub-chunk)
                (treesit-splitter-chunk-complexity-score chunk)))
        (push sub-chunk sub-chunks)))

    (nreverse sub-chunks)))

;; Multiple Fallback Strategies

(defcustom treesit-splitter-fallback-strategies
  '(imenu intelligent-line simple-line)
  "List of fallback strategies to try when tree-sitter fails.
Strategies are tried in order: 'imenu, 'intelligent-line, 'simple-line."
  :type '(repeat (choice (const :tag "Imenu integration" imenu)
                         (const :tag "Intelligent line splitting" intelligent-line)
                         (const :tag "Simple line splitting" simple-line)))
  :group 'treesit-splitter)

(defcustom treesit-splitter-context-lines 2
  "Number of lines of context to include around chunks for better understanding."
  :type 'integer
  :group 'treesit-splitter)

(defvar treesit-splitter--language-boundaries
  '((clojure . ("^\\s-*(def" "^\\s-*(ns" "^\\s-*;;"))
    (elisp . ("^\\s-*(def" "^\\s-*(provide" "^\\s-*;;;"))
    (javascript . ("^\\s-*function" "^\\s-*class" "^\\s-*export" "^\\s-*//"))
    (typescript . ("^\\s-*function" "^\\s-*class" "^\\s-*interface" "^\\s-*type" "^\\s-*export" "^\\s-*//"))
    (python . ("^\\s-*def" "^\\s-*class" "^\\s-*async def" "^\\s-*#"))
    (java . ("^\\s-*public" "^\\s-*private" "^\\s-*protected" "^\\s-*class" "^\\s-*interface"))
    (cpp . ("^\\s-*class" "^\\s-*struct" "^\\s-*namespace" "^\\s-*template" "^\\s-*/"))
    (c . ("^\\s-*static" "^\\s-*int" "^\\s-*void" "^\\s-*struct" "^\\s-*/"))
    (go . ("^\\s-*func" "^\\s-*type" "^\\s-*var" "^\\s-*const" "^\\s-*package" "^\\s-*//"))
    (rust . ("^\\s-*fn" "^\\s-*impl" "^\\s-*struct" "^\\s-*enum" "^\\s-*trait" "^\\s-*mod" "^\\s-*//")))
  "Language-specific patterns for logical boundaries.")

(defun treesit-splitter--try-fallback-strategies (content language file-path &optional original-buffer)
  "Try multiple fallback strategies for splitting CONTENT.
ORIGINAL-BUFFER is the source buffer for context."
  (let ((strategies treesit-splitter-fallback-strategies)
        chunks)
    (message "Tree-sitter failed for %s, trying fallback strategies..." language)

    (while (and strategies (null chunks))
      (let ((strategy (car strategies)))
        (setq strategies (cdr strategies))
        (message "Trying fallback strategy: %s" strategy)

        (setq chunks
              (condition-case err
                  (pcase strategy
                    ('imenu (treesit-splitter--imenu-split content language file-path original-buffer))
                    ('intelligent-line (treesit-splitter--intelligent-line-split content language file-path))
                    ('simple-line (treesit-splitter--simple-line-split content language file-path)))
                (error
                 (message "Fallback strategy %s failed: %s" strategy (error-message-string err))
                 nil)))

        (when chunks
          (message "Successfully used %s fallback strategy, found %d chunks" strategy (length chunks)))))

    (or chunks
        ;; Ultimate fallback: simple line splitting
        (progn
          (message "All strategies failed, using simple line splitting")
          (treesit-splitter--simple-line-split content language file-path)))))

(defun treesit-splitter--imenu-split (content language file-path &optional original-buffer)
  "Split CONTENT using imenu when available."
  (when (and original-buffer (buffer-live-p original-buffer))
    (with-current-buffer original-buffer
      (when (and (boundp 'imenu-create-index-function)
                 imenu-create-index-function)
        (let* ((imenu-index (condition-case nil
                                (funcall imenu-create-index-function)
                              (error nil)))
               (chunks '()))
          (when imenu-index
            (message "Using imenu index with %d entries" (length imenu-index))
            ;; Process imenu entries
            (setq chunks (treesit-splitter--process-imenu-entries
                          imenu-index content language file-path))
            ;; Add context and enhance chunks
            (when chunks
              (setq chunks (treesit-splitter--add-context-to-chunks chunks content))
              (setq chunks (treesit-splitter--enhance-fallback-chunks chunks language)))
            chunks))))))

(defun treesit-splitter--process-imenu-entries (imenu-index content language file-path)
  "Process IMENU-INDEX entries into chunks."
  (let ((chunks '())
        (lines (split-string content "\\n"))
        (total-lines (length (split-string content "\\n"))))

    ;; Sort entries by position
    (let ((sorted-entries '()))
      (dolist (entry imenu-index)
        (cond
         ;; Simple entry: (name . position)
         ((and (consp entry) (not (listp (cdr entry))))
          (push (cons (car entry) (cdr entry)) sorted-entries))
         ;; Submenu: (name (subname . position) ...)
         ((and (consp entry) (listp (cdr entry)))
          (dolist (subentry (cdr entry))
            (when (and (consp subentry) (not (listp (cdr subentry))))
              (push (cons (format "%s::%s" (car entry) (car subentry)) (cdr subentry)) sorted-entries))))))

      (setq sorted-entries (sort sorted-entries (lambda (a b) (< (cdr a) (cdr b)))))

      ;; Create chunks from sorted entries
      (let ((i 0))
        (dolist (entry sorted-entries)
          (let* ((name (car entry))
                 (pos (cdr entry))
                 (start-line (line-number-at-pos pos))
                 (end-line (if (< (1+ i) (length sorted-entries))
                               (1- (line-number-at-pos (cdr (nth (1+ i) sorted-entries))))
                             total-lines))
                 (chunk-content (mapconcat 'identity
                                           (cl-subseq lines (1- start-line) (min end-line total-lines))
                                           "\\n")))
            (when (and chunk-content
                       (> (length (string-trim chunk-content)) treesit-splitter-min-chunk-size))
              (push (make-treesit-splitter-chunk
                     :content (string-trim chunk-content)
                     :start-line start-line
                     :end-line end-line
                     :language language
                     :file-path file-path
                     :node-type "imenu"
                     :definition-name name
                     :definition-type "imenu-entry")
                    chunks))
            (setq i (1+ i)))))

      (nreverse chunks))))

(defun treesit-splitter--intelligent-line-split (content language file-path)
  "Split CONTENT using intelligent line boundary detection."
  (let* ((lines (split-string content "\\n"))
         (boundaries (treesit-splitter--get-language-boundaries language))
         (chunks '())
         (current-chunk-lines '())
         (current-start-line 1)
         (line-num 1))

    (dolist (line lines)
      (let ((is-boundary (treesit-splitter--line-matches-boundaries line boundaries))
            (current-size (length (mapconcat 'identity current-chunk-lines "\\n"))))

        ;; Check if we should start a new chunk
        (when (and is-boundary
                   current-chunk-lines
                   (> current-size treesit-splitter-min-chunk-size))
          ;; Create chunk from accumulated lines
          (let ((chunk-content (mapconcat 'identity current-chunk-lines "\\n")))
            (push (make-treesit-splitter-chunk
                   :content (string-trim chunk-content)
                   :start-line current-start-line
                   :end-line (1- line-num)
                   :language language
                   :file-path file-path
                   :node-type "intelligent-boundary"
                   :definition-name (treesit-splitter--extract-definition-from-line line language)
                   :definition-type "auto-detected")
                  chunks))
          ;; Start new chunk
          (setq current-chunk-lines (list line)
                current-start-line line-num))

        ;; If not a boundary, add line to current chunk
        (unless is-boundary
          (unless current-chunk-lines
            (setq current-chunk-lines '()))
          (push line current-chunk-lines))

        ;; Check if current chunk is too large
        (when (and (not is-boundary)
                   (> (+ current-size (length line)) treesit-splitter-chunk-size))
          ;; Force split here
          (let ((chunk-content (mapconcat 'identity current-chunk-lines "\\n")))
            (when (> (length (string-trim chunk-content)) treesit-splitter-min-chunk-size)
              (push (make-treesit-splitter-chunk
                     :content (string-trim chunk-content)
                     :start-line current-start-line
                     :end-line (1- line-num)
                     :language language
                     :file-path file-path
                     :node-type "size-boundary")
                    chunks)))
          ;; Start new chunk
          (setq current-chunk-lines (list line)
                current-start-line line-num))

        ;; Add line to current chunk if not starting new
        (unless (and is-boundary current-chunk-lines (not (equal current-chunk-lines (list line))))
          (unless current-chunk-lines
            (setq current-chunk-lines '()))
          (push line current-chunk-lines))

        (setq line-num (1+ line-num))))

    ;; Handle remaining lines
    (when current-chunk-lines
      (let ((chunk-content (mapconcat 'identity (nreverse current-chunk-lines) "\\n")))
        (when (> (length (string-trim chunk-content)) treesit-splitter-min-chunk-size)
          (push (make-treesit-splitter-chunk
                 :content (string-trim chunk-content)
                 :start-line current-start-line
                 :end-line line-num
                 :language language
                 :file-path file-path
                 :node-type "final-chunk")
                chunks))))

    (nreverse chunks)))

(defun treesit-splitter--get-language-boundaries (language)
  "Get boundary patterns for LANGUAGE."
  (or (alist-get language treesit-splitter--language-boundaries)
      ;; Generic fallback patterns
      '("^\\s-*function" "^\\s-*class" "^\\s-*def" "^\\s-*//" "^\\s-*#")))

(defun treesit-splitter--line-matches-boundaries (line boundaries)
  "Check if LINE matches any of the BOUNDARIES patterns."
  (cl-some (lambda (pattern)
             (string-match-p pattern line))
           boundaries))

(defun treesit-splitter--extract-definition-from-line (line language)
  "Extract definition name from LINE for LANGUAGE."
  (pcase language
    ('clojure
     (when (string-match "^\\s-*(\\(def[a-z]*\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" line)
       (match-string 2 line)))
    ('elisp
     (when (string-match "^\\s-*(\\(def[a-z]*\\)\\s-+\\([a-zA-Z_-][a-zA-Z0-9_-]*\\)" line)
       (match-string 2 line)))
    ((or 'javascript 'typescript)
     (cond
      ((string-match "^\\s-*function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" line)
       (match-string 1 line))
      ((string-match "^\\s-*class\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" line)
       (match-string 1 line))
      ((string-match "^\\s-*export\\s-+\\(?:function\\|class\\)\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" line)
       (match-string 1 line))))
    ('python
     (cond
      ((string-match "^\\s-*def\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" line)
       (match-string 1 line))
      ((string-match "^\\s-*class\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" line)
       (match-string 1 line))))
    (_ nil)))

(defun treesit-splitter--add-context-to-chunks (chunks content)
  "Add context lines around CHUNKS from CONTENT."
  (when (> treesit-splitter-context-lines 0)
    (let ((lines (split-string content "\\n"))
          (total-lines (length (split-string content "\\n"))))
      (mapcar (lambda (chunk)
                (let* ((start-line (treesit-splitter-chunk-start-line chunk))
                       (end-line (treesit-splitter-chunk-end-line chunk))
                       (context-start (max 1 (- start-line treesit-splitter-context-lines)))
                       (context-end (min total-lines (+ end-line treesit-splitter-context-lines)))
                       (context-content (mapconcat 'identity
                                                   (cl-subseq lines (1- context-start) context-end)
                                                   "\\n")))
                  (setf (treesit-splitter-chunk-content chunk) context-content)
                  (setf (treesit-splitter-chunk-start-line chunk) context-start)
                  (setf (treesit-splitter-chunk-end-line chunk) context-end)
                  chunk))
              chunks)))
  chunks)

(defun treesit-splitter--enhance-fallback-chunks (chunks language)
  "Enhance fallback CHUNKS with metadata for LANGUAGE."
  (mapcar (lambda (chunk)
            (let* ((content (treesit-splitter-chunk-content chunk))
                   (complexity (treesit-splitter--calculate-complexity content))
                   (symbols-used (treesit-splitter--extract-symbols-used content language))
                   (checksum (secure-hash 'md5 content)))
              (setf (treesit-splitter-chunk-complexity-score chunk) complexity)
              (setf (treesit-splitter-chunk-symbols-used chunk) symbols-used)
              (setf (treesit-splitter-chunk-checksum chunk) checksum)
              chunk))
          chunks))

(defun treesit-splitter--simple-line-split (content language file-path)
  "Simple line-based splitting when all else fails."
  (let* ((lines (split-string content "\\n" t))
         (chunks '())
         (current-chunk "")
         (current-start-line 1)
         (current-line-count 0))

    (dolist (line lines)
      (let ((line-with-newline (concat line "\\n")))
        (if (and (> (length current-chunk) 0)
                 (> (+ (length current-chunk) (length line-with-newline))
                    treesit-splitter-chunk-size))
            ;; Create a chunk
            (progn
              (push (make-treesit-splitter-chunk
                     :content (string-trim current-chunk)
                     :start-line current-start-line
                     :end-line (+ current-start-line current-line-count -1)
                     :language language
                     :file-path file-path
                     :node-type "simple-line")
                    chunks)
              (setq current-chunk line-with-newline
                    current-start-line (+ current-start-line current-line-count)
                    current-line-count 1))
          ;; Add line to current chunk
          (setq current-chunk (concat current-chunk line-with-newline)
                current-line-count (1+ current-line-count)))))

    ;; Add the last chunk
    (when (> (length (string-trim current-chunk)) 0)
      (push (make-treesit-splitter-chunk
             :content (string-trim current-chunk)
             :start-line current-start-line
             :end-line (+ current-start-line current-line-count -1)
             :language language
             :file-path file-path
             :node-type "simple-line")
            chunks))

    (nreverse chunks)))

(defun treesit-splitter--add-overlap (chunks)
  "Add overlap between CHUNKS."
  (if (or (<= (length chunks) 1)
          (<= treesit-splitter-chunk-overlap 0))
      chunks
    (let ((overlapped-chunks '()))
      (dotimes (i (length chunks))
        (let* ((chunk (nth i chunks))
               (content (treesit-splitter-chunk-content chunk))
               (metadata (copy-treesit-splitter-chunk chunk)))

          ;; Add overlap from previous chunk
          (when (> i 0)
            (let* ((prev-chunk (nth (1- i) chunks))
                   (prev-content (treesit-splitter-chunk-content prev-chunk))
                   (overlap-text (substring prev-content
                                            (max 0 (- (length prev-content)
                                                      treesit-splitter-chunk-overlap)))))
              (setf (treesit-splitter-chunk-content metadata)
                    (concat overlap-text "\n" content))))

          (push metadata overlapped-chunks)))
      (nreverse overlapped-chunks))))

(defun treesit-splitter--fallback-split (content language file-path)
  "Fallback to simple line-based splitting when AST parsing fails."
  (let* ((lines (split-string content "\n" t))
         (chunks '())
         (current-chunk "")
         (current-start-line 1)
         (current-line-count 0))

    (dolist (line lines)
      (let ((line-with-newline (concat line "\n")))
        (if (and (> (length current-chunk) 0)
                 (> (+ (length current-chunk) (length line-with-newline))
                    treesit-splitter-chunk-size))
            ;; Create a chunk
            (progn
              (push (make-treesit-splitter-chunk
                     :content (string-trim current-chunk)
                     :start-line current-start-line
                     :end-line (+ current-start-line current-line-count -1)
                     :language language
                     :file-path file-path
                     :node-type "fallback")
                    chunks)
              (setq current-chunk line-with-newline
                    current-start-line (+ current-start-line current-line-count)
                    current-line-count 1))
          ;; Add line to current chunk
          (setq current-chunk (concat current-chunk line-with-newline)
                current-line-count (1+ current-line-count)))))

    ;; Add the last chunk
    (when (> (length (string-trim current-chunk)) 0)
      (push (make-treesit-splitter-chunk
             :content (string-trim current-chunk)
             :start-line current-start-line
             :end-line (+ current-start-line current-line-count -1)
             :language language
             :file-path file-path
             :node-type "fallback")
            chunks))

    (nreverse chunks)))

;;;###autoload
(defun treesit-splitter-split-buffer (&optional language file-path)
  "Split current buffer into chunks using tree-sitter.
LANGUAGE is the programming language (auto-detected if nil).
FILE-PATH is the file path for metadata (uses buffer-file-name if nil)."
  (interactive)
  (let* ((language (or language
                       (treesit-splitter--detect-language)))
         (file-path (or file-path
                        (buffer-file-name)
                        (buffer-name)))
         (content (buffer-string))
         (original-buffer (current-buffer)))
    (treesit-splitter-split-string content language file-path original-buffer)))

;;;###autoload
(defun treesit-splitter-split-string (content language &optional file-path original-buffer)
  "Split CONTENT string into chunks using tree-sitter.
LANGUAGE is the programming language.
FILE-PATH is optional file path for metadata.
ORIGINAL-BUFFER is the source buffer for LSP context."
  (let ((normalized-lang (treesit-splitter--normalize-language language)))
    (if (not (treesit-splitter--language-supported-p normalized-lang))
        (progn
          (message "Language %s not supported by tree-sitter splitter, using fallback strategies" language)
          (treesit-splitter--try-fallback-strategies content normalized-lang file-path original-buffer))

      (with-temp-buffer
        (insert content)
        (condition-case err
            (let* ((parser (treesit-parser-create normalized-lang))
                   (root-node (treesit-parser-root-node parser))
                   (splittable-types (treesit-splitter--get-splittable-types normalized-lang)))

              (unless root-node
                (error "Failed to parse AST"))

              (message "Using tree-sitter splitter for %s" language)

              ;; Extract chunks based on AST nodes
              (let ((chunks (treesit-splitter--extract-ast-chunks
                             root-node splittable-types normalized-lang file-path original-buffer)))

                ;; If no meaningful chunks found, create a single chunk
                (when (null chunks)
                  (setq chunks (list (make-treesit-splitter-chunk
                                      :content content
                                      :start-line 1
                                      :end-line (line-number-at-pos (point-max))
                                      :language normalized-lang
                                      :file-path file-path
                                      :node-type "whole-file"))))

                ;; Split large chunks and add overlap
                (let ((refined-chunks '()))
                  (dolist (chunk chunks)
                    (if (<= (length (treesit-splitter-chunk-content chunk))
                            treesit-splitter-chunk-size)
                        (push chunk refined-chunks)
                      (setq refined-chunks
                            (append (treesit-splitter--split-large-chunk chunk)
                                    refined-chunks))))

                  (treesit-splitter--add-overlap (nreverse refined-chunks)))))

          (error
           (message "Tree-sitter splitter failed: %s, using fallback strategies" (error-message-string err))
           (treesit-splitter--try-fallback-strategies content normalized-lang file-path original-buffer)))))))

;; LSP Enhancement Functions

(defun treesit-splitter--lsp-available-p (&optional buffer)
  "Check if LSP is available and connected in BUFFER (or current buffer)."
  (with-current-buffer (or buffer (current-buffer))
    (and (featurep 'lsp-mode)
         (bound-and-true-p lsp-mode)
         (lsp-workspaces))))

(defun treesit-splitter--get-lsp-hover-info (pos &optional buffer)
  "Get LSP hover information at POS in BUFFER (or current buffer)."
  (when (treesit-splitter--lsp-available-p buffer)
    (with-current-buffer (or buffer (current-buffer))
      (condition-case nil
          (save-excursion
            (goto-char pos)
            (let ((hover-info (lsp-request "textDocument/hover"
                                           (lsp--text-document-position-params))))
              (when hover-info
                (let ((contents (lsp:hover-contents hover-info)))
                  (cond
                   ((lsp-marked-string? contents)
                    (lsp:marked-string-value contents))
                   ((vectorp contents)
                    (mapconcat (lambda (item)
                                 (if (lsp-marked-string? item)
                                     (lsp:marked-string-value item)
                                   (when (hash-table-p item)
                                     (gethash "value" item))))
                               contents "\n"))
                   ((hash-table-p contents)
                    (gethash "value" contents))
                   (t (format "%s" contents)))))))
        (error nil)))))

(defun treesit-splitter--get-lsp-symbol-info (pos &optional buffer)
  "Get LSP symbol information at POS in BUFFER (or current buffer)."
  (when (treesit-splitter--lsp-available-p buffer)
    (with-current-buffer (or buffer (current-buffer))
      (condition-case nil
          (save-excursion
            (goto-char pos)
            (let* ((symbols (lsp-request "textDocument/documentSymbol"
                                         (lsp--text-document-identifier)))
                   (line-col (cons (1- (line-number-at-pos)) (current-column)))
                   (target-line (car line-col))
                   (target-col (cdr line-col)))
              (treesit-splitter--find-symbol-at-position symbols target-line target-col)))
        (error nil)))))

(defun treesit-splitter--find-symbol-at-position (symbols line col)
  "Find symbol at LINE and COL in SYMBOLS list."
  (catch 'found
    (dolist (symbol symbols)
      (let* ((range (lsp:symbol-information-location symbol))
             (start-line (lsp:position-line (lsp:range-start range)))
             (end-line (lsp:position-line (lsp:range-end range)))
             (start-col (lsp:position-character (lsp:range-start range)))
             (end-col (lsp:position-character (lsp:range-end range))))
        (when (and (>= line start-line) (<= line end-line)
                   (or (> line start-line) (>= col start-col))
                   (or (< line end-line) (<= col end-col)))
          (throw 'found symbol))
        ;; Check children if any
        (when-let ((children (lsp:document-symbol-children symbol)))
          (let ((found (treesit-splitter--find-symbol-at-position children line col)))
            (when found (throw 'found found))))))))

(defun treesit-splitter--extract-function-name (content node-type)
  "Extract function/symbol name from CONTENT based on NODE-TYPE."
  (pcase node-type
    ("function_declaration"
     ;; Handle: function name(...) and async function name(...)
     (cond
      ((string-match "\\(?:async\\s-+\\)?function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))
      ;; Handle export function name(...)
      ((string-match "export\\s-+\\(?:async\\s-+\\)?function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))))
    ("export_statement"
     ;; Handle TypeScript/JavaScript export statements
     (cond
      ;; export function name(...)
      ((string-match "export\\s-+\\(?:async\\s-+\\)?function\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))
      ;; export const name = ...
      ((string-match "export\\s-+const\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))
      ;; export class Name
      ((string-match "export\\s-+class\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))
      ;; export interface Name
      ((string-match "export\\s-+interface\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))
      ;; export type Name
      ((string-match "export\\s-+type\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))))
    ("interface_declaration"
     ;; Handle TypeScript interfaces
     (when (string-match "interface\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content)))
    ("type_alias_declaration"
     ;; Handle TypeScript type aliases
     (when (string-match "type\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content)))
    ("arrow_function"
     (cond
      ;; Handle: const name = (...) => or const name = async (...) =>
      ((string-match "\\(const\\|let\\|var\\)\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*=\\s-*\\(?:async\\s-+\\)?(" content)
       (match-string 2 content))
      ;; Handle: export const name = (...) =>
      ((string-match "export\\s-+\\(const\\|let\\|var\\)\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*=" content)
       (match-string 2 content))))
    ("class_declaration"
     ;; Handle: class Name and export class Name
     (cond
      ((string-match "\\(?:export\\s-+\\)?class\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (match-string 1 content))))
    ("method_definition"
     (cond
      ;; Handle: methodName(...) or async methodName(...)
      ((string-match "\\(?:async\\s-+\\)?\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*(" content)
       (match-string 1 content))
      ;; Handle: constructor(...)
      ((string-match "constructor\\s-*(" content)
       "constructor")
      ;; Handle: get/set accessors
      ((string-match "\\(get\\|set\\)\\s-+\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content)
       (concat (match-string 1 content) " " (match-string 2 content)))))
    ("function_definition"
     ;; Python: def name(...) or async def name(...)
     (when (string-match "\\(?:async\\s-+\\)?def\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)" content)
       (match-string 1 content)))
    ;; Clojure forms
    ("defn"
     (when (string-match "(defn\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" content)
       (match-string 1 content)))
    ("defmacro"
     (when (string-match "(defmacro\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" content)
       (match-string 1 content)))
    ("defprotocol"
     (when (string-match "(defprotocol\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" content)
       (match-string 1 content)))
    ("defrecord"
     (when (string-match "(defrecord\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" content)
       (match-string 1 content)))
    ("deftype"
     (when (string-match "(deftype\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" content)
       (match-string 1 content)))
    ("def"
     (when (string-match "(def\\s-+\\([a-zA-Z_][a-zA-Z0-9_*+\\-]*\\)" content)
       (match-string 1 content)))
    ("ns"
     (when (string-match "(ns\\s-+\\([a-zA-Z_][a-zA-Z0-9_.\\-]*\\)" content)
       (match-string 1 content)))
    (_ nil)))

(defun treesit-splitter--extract-symbols-used (content language)
  "Extract symbols used in CONTENT for LANGUAGE."
  (let ((symbols '()))
    (pcase language
      ('clojure
       ;; Extract Clojure function calls and symbols
       (let ((pos 0))
         (while (string-match "(\\([a-zA-Z_][a-zA-Z0-9_*+\\-/]*\\)" content pos)
           (let ((symbol (match-string 1 content)))
             (unless (member symbol '("defn" "defmacro" "def" "let" "if" "when" "cond"))
               (push symbol symbols)))
           (setq pos (match-end 0)))))
      ((or 'javascript 'typescript)
       ;; Extract JavaScript/TypeScript function calls and method calls
       (let ((pos 0))
         (while (string-match "\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)\\s-*(" content pos)
           (let ((symbol (match-string 1 content)))
             (unless (member symbol '("if" "for" "while" "switch" "catch" "function"))
               (push symbol symbols)))
           (setq pos (match-end 0)))
         ;; Also extract property access
         (setq pos 0)
         (while (string-match "\\.\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" content pos)
           (push (match-string 1 content) symbols)
           (setq pos (match-end 0)))
         ;; Extract imports
         (setq pos 0)
         (while (string-match "import\\s-+{?\\s-*\\([^}]+\\)\\s-*}?\\s-+from" content pos)
           (let ((import-names (split-string (match-string 1 content) "[, ]+" t)))
             (dolist (name import-names)
               (when (string-match "\\([a-zA-Z_$][a-zA-Z0-9_$]*\\)" name)
                 (push (match-string 1 name) symbols))))
           (setq pos (match-end 0)))))
      ('python
       ;; Extract Python function calls
       (let ((pos 0))
         (while (string-match "\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\s-*(" content pos)
           (let ((symbol (match-string 1 content)))
             (unless (member symbol '("if" "for" "while" "def" "class"))
               (push symbol symbols)))
           (setq pos (match-end 0))))))
    (delete-dups symbols)))

(defun treesit-splitter--count-matches (regexp string)
  "Count matches of REGEXP in STRING."
  (let ((count 0)
        (pos 0))
    (while (string-match regexp string pos)
      (setq count (1+ count)
            pos (match-end 0)))
    count))

(defun treesit-splitter--calculate-complexity (content)
  "Calculate rough complexity score for CONTENT."
  (let ((complexity 1))
    ;; Count control structures
    (setq complexity (+ complexity
                        (* 2 (treesit-splitter--count-matches "\\(if\\|when\\|cond\\|case\\|switch\\)" content))
                        (* 3 (treesit-splitter--count-matches "\\(for\\|while\\|loop\\|doseq\\)" content))
                        (* 1 (treesit-splitter--count-matches "\\(and\\|or\\|&&\\|||\\)" content))))
    ;; Factor in length
    (setq complexity (+ complexity (/ (length content) 100)))
    complexity))

(defun treesit-splitter--enhance-chunk-with-lsp (chunk pos &optional original-buffer)
  "Enhance CHUNK with LSP metadata at POS using ORIGINAL-BUFFER for LSP context."
  (let* ((content (treesit-splitter-chunk-content chunk))
         (node-type (treesit-splitter-chunk-node-type chunk))
         (language (treesit-splitter-chunk-language chunk))
         ;; Extract basic information
         (def-name (treesit-splitter--extract-function-name content node-type))
         (symbols-used (treesit-splitter--extract-symbols-used content language))
         (complexity (treesit-splitter--calculate-complexity content))
         (checksum (secure-hash 'md5 content))
         ;; Get LSP information from original buffer
         (hover-info (treesit-splitter--get-lsp-hover-info pos original-buffer))
         (symbol-info (treesit-splitter--get-lsp-symbol-info pos original-buffer)))

    ;; Set enhanced metadata
    (setf (treesit-splitter-chunk-definition-name chunk) def-name)
    (setf (treesit-splitter-chunk-definition-type chunk)
          (treesit-splitter--normalize-definition-type node-type))
    (setf (treesit-splitter-chunk-hover-info chunk) hover-info)
    (setf (treesit-splitter-chunk-symbols-used chunk) symbols-used)
    (setf (treesit-splitter-chunk-complexity-score chunk) complexity)
    (setf (treesit-splitter-chunk-checksum chunk) checksum)

    ;; Extract signature and docstring from LSP if available
    (when symbol-info
      (setf (treesit-splitter-chunk-signature chunk)
            (lsp:symbol-information-name symbol-info)))

    ;; Extract docstring from content
    (setf (treesit-splitter-chunk-docstring chunk)
          (treesit-splitter--extract-docstring content language))))

(defun treesit-splitter--normalize-definition-type (node-type)
  "Normalize NODE-TYPE to standard definition types."
  (pcase node-type
    ((or "function_declaration" "arrow_function" "function_definition" "defn") "function")
    ((or "class_declaration" "class_specifier") "class")
    ((or "method_definition" "method_declaration") "method")
    ((or "interface_declaration" "defprotocol") "interface")
    ("type_alias_declaration" "type")
    ("export_statement" "export")
    ("defmacro" "macro")
    ("ns" "namespace")
    ("def" "variable")
    (_ node-type)))

(defun treesit-splitter--extract-docstring (content language)
  "Extract docstring from CONTENT for LANGUAGE."
  (pcase language
    ('clojure
     ;; Clojure docstrings come after function name and args
     (when (string-match "(def\\(?:n\\|macro\\)\\s-+[^\"]*\"\\([^\"]*\\)\"" content)
       (match-string 1 content)))
    ('python
     ;; Python docstrings are triple-quoted strings at function start
     (cond
      ((string-match "def\\s-+[^:]+:\\s*\"\"\"\\([^\"]*\\)\"\"\"" content)
       (match-string 1 content))
      ((string-match "def\\s-+[^:]+:\\s*'''\\([^']*\\)'''" content)
       (match-string 1 content))))
    ((or 'javascript 'typescript)
     ;; JSDoc comments before function
     (cond
      ((string-match "/\\*\\*\\([^*]*\\(?:\\*[^/][^*]*\\)*\\)\\*/" content)
       (string-trim (replace-regexp-in-string "\\*" "" (match-string 1 content))))
      ;; Single line comments before function
      ((string-match "//\\s-*\\([^\n]+\\)\\s*\\n.*function\\|//\\s-*\\([^\n]+\\)\\s*\\n.*=>" content)
       (or (match-string 1 content) (match-string 2 content)))))
    (_ nil)))

(defun treesit-splitter--detect-language ()
  "Detect programming language from current buffer."
  (or (when buffer-file-name
        (let ((ext (file-name-extension buffer-file-name)))
          (cond
           ((member ext '("js" "mjs" "jsx")) 'javascript)
           ((member ext '("ts" "tsx")) 'typescript)
           ((member ext '("py" "pyw")) 'python)
           ((equal ext "java") 'java)
           ((member ext '("cpp" "cc" "cxx" "c++")) 'cpp)
           ((equal ext "c") 'c)
           ((equal ext "go") 'go)
           ((member ext '("rs")) 'rust)
           ((equal ext "cs") 'csharp)
           ((equal ext "scala") 'scala)
           ((member ext '("clj" "cljs" "cljc")) 'clojure)
           (t nil))))
      (when (boundp 'major-mode)
        (cond
         ((memq major-mode '(js-mode javascript-mode js2-mode)) 'javascript)
         ((memq major-mode '(typescript-mode typescript-ts-mode)) 'typescript)
         ((memq major-mode '(python-mode python-ts-mode)) 'python)
         ((memq major-mode '(java-mode java-ts-mode)) 'java)
         ((memq major-mode '(c++-mode c++-ts-mode)) 'cpp)
         ((memq major-mode '(c-mode c-ts-mode)) 'c)
         ((memq major-mode '(go-mode go-ts-mode)) 'go)
         ((memq major-mode '(rust-mode rust-ts-mode)) 'rust)
         ((memq major-mode '(csharp-mode csharp-ts-mode)) 'csharp)
         ((memq major-mode '(scala-mode scala-ts-mode)) 'scala)
         ((memq major-mode '(clojure-mode clojurec-mode clojurescript-mode)) 'clojure)
         (t nil)))
      'text))

;;;###autoload
(defun treesit-splitter-split-file (file-path &optional language)
  "Split FILE-PATH into chunks using tree-sitter.
LANGUAGE is optional and will be auto-detected if nil."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((detected-lang (or language
                             (treesit-splitter--detect-language))))
      (treesit-splitter-split-string (buffer-string) detected-lang file-path))))

(defun treesit-splitter-chunk-to-plist (chunk)
  "Convert CHUNK to a plist representation."
  (list :content (treesit-splitter-chunk-content chunk)
        :start-line (treesit-splitter-chunk-start-line chunk)
        :end-line (treesit-splitter-chunk-end-line chunk)
        :language (treesit-splitter-chunk-language chunk)
        :file-path (treesit-splitter-chunk-file-path chunk)
        :node-type (treesit-splitter-chunk-node-type chunk)
        ;; Enhanced metadata
        :definition-name (treesit-splitter-chunk-definition-name chunk)
        :definition-type (treesit-splitter-chunk-definition-type chunk)
        :signature (treesit-splitter-chunk-signature chunk)
        :docstring (treesit-splitter-chunk-docstring chunk)
        :hover-info (treesit-splitter-chunk-hover-info chunk)
        :symbols-defined (treesit-splitter-chunk-symbols-defined chunk)
        :symbols-used (treesit-splitter-chunk-symbols-used chunk)
        :dependencies (treesit-splitter-chunk-dependencies chunk)
        :complexity-score (treesit-splitter-chunk-complexity-score chunk)
        :checksum (treesit-splitter-chunk-checksum chunk)
        :related-chunks (treesit-splitter-chunk-related-chunks chunk)))

(defun treesit-splitter-chunks-to-json (chunks)
  "Convert CHUNKS to JSON representation."
  (require 'json)
  (json-encode (mapcar #'treesit-splitter-chunk-to-plist chunks)))

;;;###autoload
(defun treesit-splitter-demo ()
  "Demo function to test the splitter on current buffer."
  (interactive)
  (let* ((original-buffer (current-buffer))
         (chunks (treesit-splitter-split-buffer))
         (buf (get-buffer-create "*treesit-splitter-demo*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Found %d chunks (LSP: %s):\n\n"
                      (length chunks)
                      (with-current-buffer original-buffer
                        (let ((lsp-result (treesit-splitter--lsp-available-p)))
                          (if (and lsp-result (listp lsp-result))
                              "Available" "Not Available")))))
      (dotimes (i (length chunks))
        (let ((chunk (nth i chunks)))
          (insert (format "=== Chunk %d ===\n" (1+ i)))
          (insert (format "Lines: %d-%d\n"
                          (treesit-splitter-chunk-start-line chunk)
                          (treesit-splitter-chunk-end-line chunk)))
          (insert (format "Language: %s\n" (treesit-splitter-chunk-language chunk)))
          (insert (format "Node Type: %s\n" (treesit-splitter-chunk-node-type chunk)))
          (insert (format "Size: %d chars\n" (length (treesit-splitter-chunk-content chunk))))

          ;; Enhanced metadata display
          (when-let ((def-name (treesit-splitter-chunk-definition-name chunk)))
            (insert (format "Definition: %s (%s)\n"
                            def-name
                            (or (treesit-splitter-chunk-definition-type chunk) "unknown"))))

          (when-let ((signature (treesit-splitter-chunk-signature chunk)))
            (insert (format "Signature: %s\n" signature)))

          (when-let ((docstring (treesit-splitter-chunk-docstring chunk)))
            (insert (format "Docstring: %s\n" (substring docstring 0 (min 100 (length docstring))))))

          (when-let ((symbols (treesit-splitter-chunk-symbols-used chunk)))
            (insert (format "Symbols Used: %s\n" (mapconcat 'identity (take 5 symbols) ", "))))

          (when-let ((complexity (treesit-splitter-chunk-complexity-score chunk)))
            (insert (format "Complexity: %d\n" complexity)))

          (when-let ((hover-info (treesit-splitter-chunk-hover-info chunk)))
            (insert (format "LSP Info: %s\n" (substring hover-info 0 (min 150 (length hover-info))))))

          (insert "Content:\n")
          (insert (treesit-splitter-chunk-content chunk))
          (insert "\n\n"))))
    (pop-to-buffer buf)))

;; Additional LSP utilities

;;;###autoload
(defun treesit-splitter-analyze-file (file-path)
  "Analyze FILE-PATH and show detailed chunk information with LSP data."
  (interactive "fFile to analyze: ")
  (let* ((existing-buffer (find-buffer-visiting file-path))
         (source-buffer (or existing-buffer
                            (find-file-noselect file-path)))
         chunks
         (buf (get-buffer-create "*treesit-splitter-analysis*")))
    ;; Get chunks from the source buffer (which has LSP if available)
    (with-current-buffer source-buffer
      (setq chunks (treesit-splitter-split-buffer)))

    (with-current-buffer buf
      (erase-buffer)
      (insert (format "Analysis of %s\n" file-path))
      (insert (format "Total chunks: %d\n" (length chunks)))
      (insert (format "LSP Available: %s\n\n"
                      (with-current-buffer source-buffer
                        (if (treesit-splitter--lsp-available-p) "Yes" "No"))))

      ;; Summary statistics
      (let ((total-complexity 0)
            (avg-size 0)
            (function-count 0)
            (class-count 0))
        (dolist (chunk chunks)
          (when-let ((complexity (treesit-splitter-chunk-complexity-score chunk)))
            (setq total-complexity (+ total-complexity complexity)))
          (setq avg-size (+ avg-size (length (treesit-splitter-chunk-content chunk))))
          (when (equal (treesit-splitter-chunk-definition-type chunk) "function")
            (setq function-count (1+ function-count)))
          (when (equal (treesit-splitter-chunk-definition-type chunk) "class")
            (setq class-count (1+ class-count))))

        (insert (format "Average chunk size: %d chars\n" (/ avg-size (max 1 (length chunks)))))
        (insert (format "Total complexity: %d\n" total-complexity))
        (insert (format "Functions: %d, Classes: %d\n\n" function-count class-count)))

      ;; Detailed chunk information
      (insert "=== Detailed Chunk Information ===\n\n")
      (dotimes (i (length chunks))
        (let ((chunk (nth i chunks)))
          (insert (format "Chunk %d: %s\n"
                          (1+ i)
                          (or (treesit-splitter-chunk-definition-name chunk) "unnamed")))
          (insert (format "  Type: %s, Lines: %d-%d, Complexity: %d\n"
                          (or (treesit-splitter-chunk-definition-type chunk) "unknown")
                          (treesit-splitter-chunk-start-line chunk)
                          (treesit-splitter-chunk-end-line chunk)
                          (or (treesit-splitter-chunk-complexity-score chunk) 0)))
          (when-let ((symbols (treesit-splitter-chunk-symbols-used chunk)))
            (insert (format "  Uses: %s\n" (mapconcat 'identity (take 3 symbols) ", "))))
          (insert "\n"))))
    (pop-to-buffer buf)

    ;; Clean up: kill buffer if we created it
    (unless existing-buffer
      (kill-buffer source-buffer))))

(provide 'treesit-splitter)
