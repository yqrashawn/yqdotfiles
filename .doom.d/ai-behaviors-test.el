;;; ai-behaviors-test.el --- Tests for ai-behaviors.el -*- lexical-binding: t; -*-

(require 'ert)

;;; State tests

(ert-deftest +ai-behaviors-toggle-mode-selects ()
  "Toggling a mode when none selected should select it."
  (let ((+ai-behaviors--mode nil))
    (+ai-behaviors--toggle-mode "code")
    (should (equal +ai-behaviors--mode "code"))))

(ert-deftest +ai-behaviors-toggle-mode-deselects ()
  "Toggling the same mode again should deselect it."
  (let ((+ai-behaviors--mode "code"))
    (+ai-behaviors--toggle-mode "code")
    (should (null +ai-behaviors--mode))))

(ert-deftest +ai-behaviors-toggle-mode-exclusive ()
  "Selecting a new mode should replace the previous one."
  (let ((+ai-behaviors--mode "code"))
    (+ai-behaviors--toggle-mode "debug")
    (should (equal +ai-behaviors--mode "debug"))))

(ert-deftest +ai-behaviors-toggle-tag-adds ()
  "Toggling a tag when not present should add it."
  (let ((+ai-behaviors--tags nil))
    (+ai-behaviors--toggle-tag "deep")
    (should (member "deep" +ai-behaviors--tags))))

(ert-deftest +ai-behaviors-toggle-tag-removes ()
  "Toggling a tag when present should remove it."
  (let ((+ai-behaviors--tags '("deep" "wide")))
    (+ai-behaviors--toggle-tag "deep")
    (should-not (member "deep" +ai-behaviors--tags))
    (should (member "wide" +ai-behaviors--tags))))

(ert-deftest +ai-behaviors-toggle-tag-no-duplicates ()
  "Toggling a tag twice should not create duplicates."
  (let ((+ai-behaviors--tags nil))
    (+ai-behaviors--toggle-tag "deep")
    (+ai-behaviors--toggle-tag "deep")
    (+ai-behaviors--toggle-tag "deep")
    (should (= 1 (length (cl-remove-if-not
                           (lambda (x) (equal x "deep"))
                           +ai-behaviors--tags))))))

(ert-deftest +ai-behaviors-reset-clears-all ()
  "Reset should clear mode and tags."
  (let ((+ai-behaviors--mode "code")
        (+ai-behaviors--tags '("deep" "wide")))
    (+ai-behaviors--reset)
    (should (null +ai-behaviors--mode))
    (should (null +ai-behaviors--tags))))

;;; Format tests

(ert-deftest +ai-behaviors-format-empty ()
  "Format with no selection should return empty string."
  (let ((+ai-behaviors--mode nil)
        (+ai-behaviors--tags nil))
    (should (string-empty-p (+ai-behaviors--format)))))

(ert-deftest +ai-behaviors-format-mode-only ()
  "Format with only mode should return #=mode."
  (let ((+ai-behaviors--mode "code")
        (+ai-behaviors--tags nil))
    (should (equal "#=code" (+ai-behaviors--format)))))

(ert-deftest +ai-behaviors-format-tags-only ()
  "Format with only tags should return space-separated #tags."
  (let ((+ai-behaviors--mode nil)
        (+ai-behaviors--tags '("wide" "deep")))
    (should (equal "#deep #wide" (+ai-behaviors--format)))))

(ert-deftest +ai-behaviors-format-mode-and-tags ()
  "Format with mode and tags should put mode first."
  (let ((+ai-behaviors--mode "review")
        (+ai-behaviors--tags '("challenge" "deep")))
    (should (equal "#=review #deep #challenge" (+ai-behaviors--format)))))

(ert-deftest +ai-behaviors-format-preserves-insertion-order ()
  "Tags should appear in the order they were added."
  (let ((+ai-behaviors--mode nil)
        (+ai-behaviors--tags nil))
    (+ai-behaviors--toggle-tag "deep")
    (+ai-behaviors--toggle-tag "challenge")
    (+ai-behaviors--toggle-tag "simulate")
    (should (equal "#deep #challenge #simulate" (+ai-behaviors--format)))))

;;; Preset tests

(ert-deftest +ai-behaviors-preset-suffixes-count ()
  "Should generate one suffix per preset."
  (let ((+ai-behaviors--presets
         '(("a" "#=code #deep" "A")
           ("b" "#=review #wide" "B"))))
    (should (= 2 (length (+ai-behaviors--preset-suffixes))))))

(ert-deftest +ai-behaviors-preset-suffixes-keys ()
  "Preset suffixes should use uppercase letter keys."
  (let ((+ai-behaviors--presets
         '(("a" "#=code #deep" "A")
           ("b" "#=review #wide" "B"))))
    (let ((suffixes (+ai-behaviors--preset-suffixes)))
      (should (equal "A" (car (nth 0 suffixes))))
      (should (equal "B" (car (nth 1 suffixes)))))))

(ert-deftest +ai-behaviors-preset-suffixes-empty ()
  "No presets should produce empty list."
  (let ((+ai-behaviors--presets nil))
    (should (null (+ai-behaviors--preset-suffixes)))))

;;; ai-behaviors-test.el ends here
