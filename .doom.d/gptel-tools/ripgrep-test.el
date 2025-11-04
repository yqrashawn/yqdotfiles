;;; ripgrep-test.el --- GPTEL ripgrep tests -*- lexical-binding: t; -*-
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Homepage: https://github.com/yqrashawn/gptel-tools
;; Keywords: tools, test

;;; Commentary:
;;
;; Tests for GPTEL ripgrep tool.
;;
;;; Code:

(require 'ert)

(ert-deftest gptelt-rg-tool-glob-el-files-test ()
  "Test glob finds at least one el file in project."
  (let ((results (gptelt-rg-tool-glob "*.el" "project" 10)))
    (should (and (listp results)
                 results
                 (seq-some (lambda (f) (string-match-p "\\.el$" f)) results)))))

(ert-deftest gptelt-rg-tool-search-content-known-string-test ()
  "Test search-content finds a known string in this file."
  (let* ((thisfile (expand-file-name "~/.nixpkgs/.doom.d/init.el"))
         (needle "lexical-binding")
         (results (gptelt-rg-tool-search-content
                   needle (file-name-directory thisfile)
                   "*.el" nil nil 0 10)))
    (should (seq-some (lambda (r) (string-match-p needle (plist-get r :content))) results))))

(ert-deftest gptelt-rg-tool-search-regex-lexical-binding-test ()
  "Test search-regex finds 'lexical-binding' using regex."
  (let* ((thisfile (expand-file-name "~/.nixpkgs/.doom.d/init.el"))
         (results (gptelt-rg-tool-search-regex "lexical-binding" (file-name-directory thisfile) "*.el" 10)))
    (should (seq-some (lambda (r) (string-match-p "lexical-binding" (plist-get r :content))) results))))

(ert-deftest gptelt-rg-tool-glob-relative-dir-pattern-test ()
  "Test glob with relative directory pattern like 'gptel-tools/*.el'."
  (let ((results (gptelt-rg-tool-glob "gptel-tools/*.el")))
    (should (and (listp results)
                 results
                 (seq-some (lambda (f) (string-match-p "gptel-tools/.*\\.el$" f)) results)))))

(ert-deftest gptelt-rg-tool-glob-nested-dir-pattern-test ()
  "Test glob with nested directory pattern like 'src/app/*.cljs'."
  (let ((results (gptelt-rg-tool-glob "src/app/*.cljs")))
    (should (or (not results) ;; OK if no such files exist
                (seq-every-p (lambda (f) (string-match-p "src/app/.*\\.cljs$" f)) results)))))

;;; ripgrep-test.el ends here
