;;; .nixpkgs/.doom.d/gptel-tools.el -*- lexical-binding: t; -*-

(require 'gptel)
(require 'smartparens nil t)
(declare-function sp-region-ok-p "smartparens")

;;; End

(load! "gptel-tools/buffer.el")
(load! "gptel-tools/create-file.el")
(load! "gptel-tools/edit-file.el")
(load! "gptel-tools/ripgrep.el")
(load! "gptel-tools/elisp.el")
