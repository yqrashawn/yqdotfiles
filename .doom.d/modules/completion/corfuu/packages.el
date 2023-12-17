(package! corfu
  :recipe (:type git :repo "minad/corfu" :files (:defaults "extensions/*.el"))
  :pin "5c4f34b68bde51beb7edfa0d1643630358aca02a")

(when (modulep! +icons)
  (package! kind-icon))

(when (modulep! +orderless)
  (package! orderless))

(package! cape)

(package! popon
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-popon"))
(package! corfu-terminal
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(package! corfu-doc-terminal
  :recipe (:type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal.git"))
(package! yasnippet-capf :recipe (:host github :repo "elken/yasnippet-capf"))
