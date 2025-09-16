;;; .nixpkgs/.doom.d/gptel-tools/clojurescript.el -*- lexical-binding: t; -*-

(defun gptelt-cljs-ensure-helper-loaded (f)
  (gptelt-clj--eval-buffer
   (find-file-noselect"~/.nixpkgs/env/dev/cljs_helper.clj")
   t t))

;;; list all loaded namespace
(defalias 'gptelt-cljs-list-ns 'gptelt-clj-list-ns)

(comment
  (gptelt-cljs-list-ns)
  (gptelt-cljs-list-ns "ajax"))

;;; list all classpath entries
(defalias 'gptelt-cljs-list-classpath 'gptelt-clj-list-classpath)

;;; list all running builds
(defun gptelt-cljs-get-running-builds ()
  (gptelt-cljs-ensure-helper-loaded)
  (gptelt-eval--clj-string "(get-running-builds)" "cljs-helper" t))

(comment
  (gptelt-cljs-list-classpath)
  (gptelt-cljs-list-classpath "\\.jar$"))
