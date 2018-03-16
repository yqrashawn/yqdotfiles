;; no confirm before evaluate code in src block
(setq org-clock-idle-time 10)
(setq org-confirm-babel-evaluate nil)
(add-hook 'org-agenda-mode-hook 'spacemacs/org-agenda-transient-state/body)

(setq org-tag-alist '((:startgroup . nil)
                      ("work" . ?w) ("home" . ?h)
                      (:endgroup . nil)))

(bind-key "C-c w" 'hydra-org-clock/body) ;; may bind to other keys
(defhydra hydra-org-clock (:color blue :hint nil)
  "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    _i_n         _e_dit   _g_oto entry | (_z_)     _r_elative      ti_m_e
^ ^      _c_ontinue   _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
^ ^      _o_ut        ^ ^      _r_eport     |  ^ ^      _p_ause toggle
^ ^      ^ ^          ^ ^      ^ ^          |  ^ ^      _s_top
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands"))

  ("r" org-timer-start)
  ("n" org-timer-set-timer)
  ("p" org-timer-pause-or-continue)
  ;; ("a" (org-timer 16)) ; double universal argument
  ("s" org-timer-stop)

  ("m" org-timer)
  ("t" org-timer-item)
  ("z" (org-info "Timers")))

(bind-key "C-c w" 'hydra-org-clock/body)
(defhydra hydra-org-clock (:color blue :hint nil)
  "
^Clock:^ ^In/out^     ^Edit^   ^Summary^    | ^Timers:^ ^Run^           ^Insert
-^-^-----^-^----------^-^------^-^----------|--^-^------^-^-------------^------
(_?_)    _i_n         _e_dit   _g_oto entry | (_z_)     _r_elative      ti_m_e
^ ^      _c_ontinue   _q_uit   _d_isplay    |  ^ ^      cou_n_tdown     i_t_em
^ ^      _o_ut        ^ ^      _r_eport     |  ^ ^      _p_ause toggle
^ ^      ^ ^          ^ ^      ^ ^          |  ^ ^      _s_top
"
  ("i" org-clock-in)
  ("o" org-clock-out)
  ("c" org-clock-in-last)
  ("e" org-clock-modify-effort-estimate)
  ("q" org-clock-cancel)
  ("g" org-clock-goto)
  ("d" org-clock-display)
  ("r" org-clock-report)
  ("?" (org-info "Clocking commands"))

  ("r" org-timer-start)
  ("n" org-timer-set-timer)
  ("p" org-timer-pause-or-continue)
  ;; ("a" (org-timer 16)) ; double universal argument
  ("s" org-timer-stop)

  ("m" org-timer)
  ("t" org-timer-item)
  ("z" (org-info "Timers")))

(defvar blog-root "~/google/sync/blog/")
(defvar blog-file-pattern "") ;; also possible: "*.org"
(setq blog-file-pattern "") ;; also possible: "*.org"




(defun my-jump-to-lazyblorg-heading-according-to-URL-in-clipboard ()
  "Retrieves an URL from the clipboard, gets its Org-mode source,
   extracts the ID of the article and jumps to its Org-mode heading"
  (interactive)
  (let (
        ;; Getting URL from the clipboard. Since it may contain
        ;; some text properties we are using substring-no-properties
        ;; function
        (url (substring-no-properties (current-kill 0)))
        ;; This is a check string: if the URL in the clipboard
        ;; doesn't start with this, an error message is shown
        (domain "yqrashawn.com")
	      )
    ;; Check if URL string is from my domain (all other strings do
    ;; not make any sense here)
    (if (string-match (upcase domain) (upcase url))
	      ;; Retrieving content by URL into new buffer asynchronously
	      (url-retrieve url
                      ;; call this lambda function when URL content is retrieved
			                (lambda (status)
			                  ;; Extrating and preparing the ID
			                  (let* (
                               ;; Limit the ID search to the top 1000 characters of the buffer
				                       (pageheader (buffer-substring 1 1000))
				                       ;; Start index of the id
                               (start (string-match "<meta name=\"orgmode-id\" content=\"" pageheader))
                               ;; End index of the id
                               (end (string-match "\" />" pageheader start))
                               ;; Amount of characters to skip for the openning tag
                               (chars-to-skip (length "<meta name=\"orgmode-id\" content=\""))
                               ;; Extract ID
                               (lazyblorg-id (if (and start end (< start end))
                                                 ;; ... extract it and return.
                                                 (substring pageheader (+ start chars-to-skip) end)
                                               nil))
                               )
			                    (message (concat "Looking for id:" lazyblorg-id))
			                    (org-open-link-from-string (concat "ID:" lazyblorg-id))
			                    )
			                  )
			                )
	    (message (concat "Sorry: the URL \"" (substring url 0 (length domain)) "...\" doesn't contain \"" domain "\". Aborting."))
      ;;(message (concat "domain: " domain))
      ;;(message (concat "url:    " url))
	    )))

(defun sr-org-id-insert-maybe ()
  (if (y-or-n-p "Create a unique ID for this section?")
      (org-id-get-create)))
