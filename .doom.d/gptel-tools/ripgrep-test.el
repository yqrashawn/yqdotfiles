;;; .nixpkgs/.doom.d/gptel-tools/ripgrep-test.el -*- lexical-binding: t; -*-

(require 'ert)

(ert-deftest gptel-rg-tool-glob-el-files-test ()
  "Test glob finds at least one el file in project."
  (let ((results (gptel-rg-tool-glob "*.el" "project" 10)))
    (should (and (listp results)
                 results
                 (seq-some (lambda (f) (string-match-p "\\.el$" f)) results)))))

(ert-deftest gptel-rg-tool-search-content-known-string-test ()
  "Test search-content finds a known string in this file."
  (let* ((thisfile (expand-file-name "~/.nixpkgs/.doom.d/init.el"))
         (needle "lexical-binding")
         (results (gptel-rg-tool-search-content
                   needle (file-name-directory thisfile)
                   "*.el" nil nil 0 10)))
    (should (seq-some (lambda (r) (string-match-p needle (plist-get r :content))) results))))

(ert-deftest gptel-rg-tool-search-regex-lexical-binding-test ()
  "Test search-regex finds 'lexical-binding' using regex."
  (let* ((thisfile (expand-file-name "~/.nixpkgs/.doom.d/init.el"))
         (results (gptel-rg-tool-search-regex "lexical-binding" (file-name-directory thisfile) "*.el" 10)))
    (should (seq-some (lambda (r) (string-match-p "lexical-binding" (plist-get r :content))) results))))
