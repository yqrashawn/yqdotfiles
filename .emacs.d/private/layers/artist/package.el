;; packages.el --- artist Layer packages File for Spacemacs
;; Author: yqrashawn <namy.19@gmail.com>
(setq artist-packages '(artist))

(defun artist/init-artist ()
  (use-package artist
    :defer t
    :init
    (spacemacs/set-leader-keys "aA" 'artist-mode)
    :config
    (progn
      (evil-define-key 'normal artist-mode-map
        "h" 'artist-backward-char
        "j" 'artist-next-line
        "k" 'artist-previous-line
        "l" 'artist-forward-char
        "sl" 'artist-select-line-char
        "sf" 'artist-select-fill-char
        "ss" 'artist-select-spray-chars
        "so" 'artist-select-operation
        "t<" 'artist-toggle-first-arrow
        "t>" 'artist-toggle-second-arrow
        "tr" 'artist-toggle-rubber-banding
        "ts" 'artist-toggle-borderless-shapes
        "tt" 'artist-toggle-trim-line-endings
        "q" 'artist-mode-off)
      (evil-make-intercept-map artist-mode-map 'normal))))