;;; .nixpkgs/.doom.d/gptel-tools/memory.el -*- lexical-binding: t; -*-

(require 'vecdb-qdrant)

(defvar vdb-provider
  (make-vecdb-qdrant-provider
   :api-key +vecdb-qdrant-api-key
   :url +vecdb-qdrant-api-url))
