(global-set-key (kbd "C-M-S-s-s") 'ns-do-hide-emacs)
(global-set-key (kbd "s-l") 'tabbar-forward)
(global-set-key (kbd "s-h") 'tabbar-backward)
(global-set-key (kbd "s-L") 'tabbar-forward-group)
(global-set-key (kbd "s-H") 'tabbar-backward-group)
(setq tabbar-cycle-scope 'tabs)
;; Add a buffer modification state indicator in the tab label, and place a
;; space around the label to make it looks less crowd.
(defadvice tabbar-buffer-tab-label (after fixup_tab_label_space_and_flag activate)
  (setq ad-return-value
        (if (and (buffer-modified-p (tabbar-tab-value tab))
                 (buffer-file-name (tabbar-tab-value tab)))
            (concat " + " (concat ad-return-value " "))
          (concat " " (concat ad-return-value " ")))))

;; Called each time the modification state of the buffer changed.
(defun ztl-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; First-change-hook is called BEFORE the change is made.
(defun ztl-on-buffer-modification ()
  (set-buffer-modified-p t)
  (ztl-modification-state-change))
(add-hook 'after-save-hook 'ztl-modification-state-change)

;; This doesn't work for revert, I don't know.
;;(add-hook 'after-revert-hook 'ztl-modification-state-change)
(add-hook 'first-change-hook 'ztl-on-buffer-modification)

(tabbar-mode)

(defun find-git-dir (dir)
  "Search up the directory tree looking for a .git folder."
  (cond
   ((eq major-mode 'erc-mode) "ERC")
   ((eq major-mode 'dired-mode) "Dired")
   ((not dir) "process")
   ((string= dir "/") "no-git")
   ((file-exists-p (concat dir "/.git")) dir)
   (t (find-git-dir (directory-file-name (file-name-directory dir))))))
(defun git-tabbar-buffer-groups ()
  "Groups tabs in tabbar-mode by the git repository they are in."
  (list (find-git-dir (buffer-file-name (current-buffer)))))
(setq tabbar-buffer-groups-function 'git-tabbar-buffer-groups)

;; This are setting for nice tabbar items
;; to have an idea of what it looks like http://imgur.com/b0SNN
;; inspired by Amit Patel screenshot http://www.emacswiki.org/pics/static/NyanModeWithCustomBackground.png

;; Tabbar
;; Tabbar settings
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20")
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white")
(set-face-attribute
 'tabbar-selected nil
 :background "gray50"
 :foreground "black")
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil)
(set-face-attribute
 'tabbar-button nil)
(set-face-attribute
 'tabbar-separator nil
 :background "gray20")

;; Change padding of the tabs
;; we also need to set separator to avoid overlapping tabs by highlighted tabs
(custom-set-variables
 '(tabbar-separator (quote (0.1))))
;; adding spaces
(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]" (tabbar-tab-tabset tab))
                  (format "%s" (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(tabbar-mode 1)
