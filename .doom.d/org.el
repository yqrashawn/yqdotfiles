;;; org.el -*- lexical-binding: t; -*-

(after! org
  (setq! org-log-done 'time
         org-startup-with-inline-images "inlineimages"
         org-cycle-emulate-tab nil
         org-cycle-global-at-bob t
         org-hide-block-startup t))
