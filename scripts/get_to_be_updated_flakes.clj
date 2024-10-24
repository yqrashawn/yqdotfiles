(ns get-to-be-updated-flakes
  (:require
   [babashka.fs :as fs]
   [babashka.process :as p]
   [cheshire.core :as json]
   [clojure.string :as string]))

(def flakes-skip-auto-updates
  #{:emacs-overlay
    :nixpkgs
    :nixpkgs-unstable
    :emacs-custom-src})

(def dir
  (if (System/getenv "CI")
    (fs/cwd)
    (fs/expand-home "~/.nixpkgs")))

(def all-inputs
  (-> (p/sh {:dir dir} "nix flake metadata --json")
      :out
      (json/parse-string keyword)
      :locks
      :nodes
      :root
      :inputs
      keys))

(def cmd
  (->> all-inputs
       (filter #(not (flakes-skip-auto-updates %)))
       (map name)
       (string/join " --update-input ")
       (str " --update-input ")
       ;; (str "nix flake lock --commit-lock-file")
       (str "nix flake lock")))

(println all-inputs)
(p/shell cmd)
