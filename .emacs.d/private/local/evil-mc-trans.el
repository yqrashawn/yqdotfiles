;; (spacemacs|define-transient-state evil-multiple-cursor
;;   :title "Evil Multiple Cursor Transient State"
;;   :doc "\n [_n_/_p_] makr next/prev link this [_N_/_P_] skip next/prev like this [_q_] quit\n[_RET_] mark all like this         [_0_] pause                        [_9_] resume\n[_C-j_/_C-k_] make line            [_gg_/_G_] find 1st/last cursor    [_C-h_] mark here"
;;   :bindings
;;   ("RET" . evil-mc-make-all-cursors)
;;   ("C-g" . evil-mc-undo-all-cursors)
;;   ("0" . evil-mc-pause-cursors)
;;   ("9" . evil-mc-resume-cursors)
;;   ("gg" . evil-mc-make-and-goto-first-cursor)
;;   ("G" . evil-mc-make-and-goto-last-cursor)
;;   ("C-h" . evil-mc-make-cursor-here)
;;   ("C-j" . evil-mc-make-cursor-move-next-line)
;;   ("C-k" . evil-mc-make-cursor-move-prev-line)
;;   ("n" . evil-mc-make-and-goto-next-match)
;;   ("N" . evil-mc-skip-and-goto-next-match)
;;   ("p" . evil-mc-make-and-goto-prev-match)
;;   ("P" . evil-mc-skip-and-goto-prev-match)
;;   ("q" nil :exit t))
;; (spacemacs/set-leader-keys "om"
;;   'spacemacs/evil-multiple-cursor-transient-state/body)

;; (defhydra hydra-mc (evil-normal-state-map "C-n")
;;   "multiple cursor"
;;   ("RET" . evil-mc-make-all-cursors)
;;   ("C-g" . evil-mc-undo-all-cursors)
;;   ("0" . evil-mc-pause-cursors)
;;   ("9" . evil-mc-resume-cursors)
;;   ("gg" . evil-mc-make-and-goto-first-cursor)
;;   ("G" . evil-mc-make-and-goto-last-cursor)
;;   ("C-h" . evil-mc-make-cursor-here)
;;   ("C-j" . evil-mc-make-cursor-move-next-line)
;;   ("C-k" . evil-mc-make-cursor-move-prev-line)
;;   ("n" . evil-mc-make-and-goto-next-match)
;;   ("N" . evil-mc-skip-and-goto-next-match)
;;   ("p" . evil-mc-make-and-goto-prev-match)
;;   ("P" . evil-mc-skip-and-goto-prev-match)
;; )
