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
       (string/join " ")
       (str
        (if (System/getenv "CI")
          "nix flake update --commit-lock-file "
          "nix flake update "))))

(println (->> all-inputs
              (mapv str)
              (string/join "\n")))
(p/shell cmd)
