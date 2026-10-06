;;; .nixpkgs/.doom.d/mcp-test.el -*- lexical-binding: t; coding: utf-8 -*-

(require 'ert)
(require 'json)

(comment
  (ert-run-tests-interactively "mcp-test-")
  (ert "mcp-test-"))

;;; +mcp-resolve-value / +mcp-server-headers

(defvar mcp-test--dynamic nil
  "Dynamically bound in `mcp-test-resolve-value'.")

(ert-deftest mcp-test-resolve-value ()
  "A function is called, a bound symbol dereferenced, anything else is itself."
  (should (equal "plain" (+mcp-resolve-value "plain")))
  (should (equal "called" (+mcp-resolve-value (lambda () "called"))))
  ;; A DYNAMIC binding: under lexical-binding a `let' of an undeclared symbol
  ;; does not make it `boundp', so only a defvar'd one is dereferenced.
  (let ((mcp-test--dynamic "deref"))
    (should (equal "deref" (+mcp-resolve-value 'mcp-test--dynamic))))
  (should (equal nil (+mcp-resolve-value nil)))
  ;; An unbound symbol is not an error, and is not silently turned into its name.
  (should (equal 'mcp-test--never-bound
            (+mcp-resolve-value 'mcp-test--never-bound))))

(ert-deftest mcp-test-headers-absent ()
  "No :headers and no :token means no :headers key at all, not an empty one.
`+gen-mcp-json-conf' relies on nil here: `when-let*' is what keeps a server
without credentials from gaining a \"headers\": {} in the JSON."
  (should (equal nil (+mcp-server-headers (list :url "http://127.0.0.1:1/mcp"))))
  (should (equal nil (+mcp-server-headers nil))))

(ert-deftest mcp-test-headers-token ()
  "A :token becomes an Authorization bearer header, resolved first."
  (should (equal '(Authorization "Bearer abc")
            (+mcp-server-headers (list :token "abc"))))
  (should (equal '(Authorization "Bearer from-lambda")
            (+mcp-server-headers (list :token (lambda () "from-lambda"))))))

(ert-deftest mcp-test-headers-alist ()
  "An mcp.el-style :headers alist is passed through as a plist."
  (should (equal '(X-Api-Key "k1")
            (+mcp-server-headers (list :headers '(("X-Api-Key" . "k1"))))))
  (should (equal '(X-A "1" X-B "2")
            (+mcp-server-headers
              (list :headers '(("X-A" . "1") ("X-B" . "2")))))))

(ert-deftest mcp-test-headers-empty-values-dropped ()
  "An empty value is worse than a missing one: it would authenticate as \"\".
A :token lambda returns nil when its file is missing, which is exactly this
case."
  (should (equal nil (+mcp-server-headers (list :token ""))))
  (should (equal nil (+mcp-server-headers (list :token (lambda () nil)))))
  (should (equal nil (+mcp-server-headers (list :headers '(("X-Empty" . "")))))))

(ert-deftest mcp-test-headers-merge-order ()
  "Explicit headers come first, the derived Authorization last."
  (should (equal '(X-A "1" Authorization "Bearer t")
            (+mcp-server-headers
              (list :token "t" :headers '(("X-A" . "1")))))))

;;; +mcp--write-json

(ert-deftest mcp-test-write-json-preserves-non-ascii ()
  "Pretty-printing must not mangle non-ASCII text.

`json-serialize' returns a unibyte string of UTF-8 bytes; inserting it raw and
then calling `json-pretty-print-buffer' re-serializes each byte as a literal
backslash-octal escape, turning \"5.1 · x\" into \"5.1 \\302\\267 x\". That
silently corrupted every non-ASCII string in ~/.claude.json, which is read
back, modified and rewritten on every run."
  (let ((file (make-temp-file "mcp-test-" nil ".json"))
         (text "Fable 5.1 · Most capable — ok"))
    (unwind-protect
      (progn
        (+mcp--write-json file (list :description text) t)
        (let ((back (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (json-parse-buffer :object-type 'plist
                        :null-object :null
                        :false-object :json-false))))
          (should (equal text (plist-get back :description)))))
      (delete-file file))))

(ert-deftest mcp-test-write-json-round-trips-a-whole-config ()
  "Parse, modify, write, parse: the shape survives, including empty objects.
An empty JSON object parses to nil under :object-type 'plist, which is also
how nil serializes back -- worth pinning, because the alternative would be an
empty array."
  (let ((file (make-temp-file "mcp-test-" nil ".json")))
    (unwind-protect
      (let* ((original "{\"keep\":{\"a\":1},\"empty\":{},\"arr\":[1,2],\"no\":false,\"nul\":null,\"uni\":\"·\"}")
              (conf (with-temp-buffer
                      (insert original)
                      (goto-char (point-min))
                      (json-parse-buffer :object-type 'plist
                        :null-object :null
                        :false-object :json-false))))
        (plist-put conf :mcpServers (list 'srv (list :url "http://x" :type "http")))
        (+mcp--write-json file conf t)
        (let ((back (with-temp-buffer
                      (insert-file-contents file)
                      (goto-char (point-min))
                      (json-parse-buffer :object-type 'plist
                        :null-object :null
                        :false-object :json-false))))
          (should (equal '(:a 1) (plist-get back :keep)))
          (should (equal nil (plist-get back :empty)))
          (should (equal [1 2] (plist-get back :arr)))
          (should (equal :json-false (plist-get back :no)))
          (should (equal :null (plist-get back :nul)))
          (should (equal "·" (plist-get back :uni)))
          ;; Keys come back as KEYWORDS under :object-type 'plist, whatever
          ;; symbol went in -- the generator's `intern'ed names included.
          (should (equal "http://x"
                    (plist-get (plist-get (plist-get back :mcpServers) :srv) :url)))))
      (delete-file file))))

(ert-deftest mcp-test-write-json-ends-at-600 ()
  "The written file ends at 600, whether it existed before or not.

Be clear about what this does NOT check: the creation window. `set-file-modes'
alone already produces a 600 file at the end, so this test passes against the
pre-`with-file-modes' code too. Observing the window would mean racing the
write, which an ERT test should not try to do; `with-file-modes' is justified
by reading the write path, not by this assertion."
  (let* ((dir (make-temp-file "mcp-test-dir-" t))
          (fresh (expand-file-name "fresh.json" dir))
          (existing (expand-file-name "existing.json" dir)))
    (unwind-protect
      (progn
        (+mcp--write-json fresh (list :token "s3cret"))
        (should (equal "-rw-------" (file-modes-number-to-symbolic
                                      (file-modes fresh) nil)))
        (with-temp-file existing (insert "{}"))
        (set-file-modes existing #o644)
        (+mcp--write-json existing (list :token "s3cret"))
        (should (equal "-rw-------" (file-modes-number-to-symbolic
                                      (file-modes existing) nil))))
      (delete-directory dir t))))
