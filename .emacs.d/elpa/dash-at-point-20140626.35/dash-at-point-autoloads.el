;;; dash-at-point-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (directory-file-name (or (file-name-directory #$) (car load-path))))

;;;### (autoloads nil "dash-at-point" "dash-at-point.el" (22635 43793
;;;;;;  0 0))
;;; Generated autoloads from dash-at-point.el

(let ((loads (get 'dash-at-point 'custom-loads))) (if (member '"dash-at-point" loads) nil (put 'dash-at-point 'custom-loads (cons '"dash-at-point" loads))))

(defvar dash-at-point-legacy-mode nil "\
Non-nil means use the legacy mode ('dash://') to invoke Dash.
Nil means use the modern mode ('dash-plugin://').
\(This mode may remove in the future.)")

(custom-autoload 'dash-at-point-legacy-mode "dash-at-point" t)

(defvar dash-at-point-mode-alist '((actionscript-mode . "actionscript") (arduino-mode . "arduino") (c++-mode . "cpp,net,boost,qt,cvcpp,cocos2dx,c,manpages") (c-mode . "c,glib,gl2,gl3,gl4,manpages") (caml-mode . "ocaml") (clojure-mode . "clojure") (coffee-mode . "coffee") (common-lisp-mode . "lisp") (cperl-mode . "perl") (css-mode . "css,bootstrap,foundation,less,awesome,cordova,phonegap") (dart-mode . "dartlang,polymerdart,angulardart") (elixir-mode . "elixir") (emacs-lisp-mode . "elisp") (enh-ruby-mode . "ruby") (erlang-mode . "erlang") (gfm-mode . "markdown") (go-mode . "go,godoc") (groovy-mode . "groovy") (haml-mode . "haml") (haskell-mode . "haskell") (html-mode . "html,svg,css,bootstrap,foundation,awesome,javascript,jquery,jqueryui,jquerym,angularjs,backbone,marionette,meteor,moo,prototype,ember,lodash,underscore,sencha,extjs,knockout,zepto,cordova,phonegap,yui") (jade-mode . "jade") (java-mode . "java,javafx,grails,groovy,playjava,spring,cvj,processing,javadoc") (js2-mode . "javascript,backbone,angularjs") (js3-mode . "nodejs") (latex-mode . "latex") (less-css-mode . "less") (lua-mode . "lua,corona") (markdown-mode . "markdown") (nginx-mode . "nginx") (objc-mode . "cpp,iphoneos,macosx,appledoc,cocoapods,cocos2dx,cocos2d,cocos3d,kobold2d,sparrow,c,manpages") (perl-mode . "perl,manpages") (php-mode . "php,wordpress,drupal,zend,laravel,yii,joomla,ee,codeigniter,cakephp,phpunit,symfony,typo3,twig,smarty,phpp,html,mysql,sqlite,mongodb,psql,redis") (processing-mode . "processing") (puppet-mode . "puppet") (python-mode . "python3,django,twisted,sphinx,flask,tornado,sqlalchemy,numpy,scipy,saltcvp") (ruby-mode . "ruby,rubygems,rails") (rust-mode . "rust") (sass-mode . "sass,compass,bourbon,neat,css") (scala-mode . "scala,akka,playscala,scaladoc") (stylus-mode . "stylus") (tcl-mode . "tcl") (tuareg-mode . "ocaml") (twig-mode . "twig") (vim-mode . "vim") (yaml-mode . "chef,ansible")) "\
Alist which maps major modes to Dash docset tags.
Each entry is of the form (MAJOR-MODE . DOCSET-TAG) where
MAJOR-MODE is a symbol and DOCSET-TAG is corresponding tags
for one or more docsets in Dash.")

(custom-autoload 'dash-at-point-mode-alist "dash-at-point" t)

(defvar dash-at-point-mode-alist-legacy '((actionscript-mode . "actionscript") (arduino-mode . "arduino") (c++-mode . "cpp") (c-mode . "c") (caml-mode . "ocaml") (clojure-mode . "clojure") (coffee-mode . "coffee") (common-lisp-mode . "lisp") (cperl-mode . "perl") (css-mode . "css") (elixir-mode . "elixir") (emacs-lisp-mode . "elisp") (enh-ruby-mode . "ruby") (erlang-mode . "erlang") (gfm-mode . "markdown") (go-mode . "go") (groovy-mode . "groovy") (haml-mode . "haml") (haskell-mode . "haskell") (html-mode . "html") (jade-mode . "jade") (java-mode . "java") (js2-mode . "javascript") (js3-mode . "nodejs") (latex-mode . "latex") (less-css-mode . "less") (lua-mode . "lua") (markdown-mode . "markdown") (nginx-mode . "nginx") (objc-mode . "iphoneos") (perl-mode . "perl") (php-mode . "php") (processing-mode . "processing") (puppet-mode . "puppet") (python-mode . "python3") (ruby-mode . "ruby") (rust-mode . "rust") (sass-mode . "sass") (scala-mode . "scala") (stylus-mode . "stylus") (tcl-mode . "tcl") (tuareg-mode . "ocaml") (twig-mode . "twig") (vim-mode . "vim") (yaml-mode . "chef")) "\
Alist which maps major modes to Dash docset tags.
Each entry is of the form (MAJOR-MODE . DOCSET-TAG) where
MAJOR-MODE is a symbol and DOCSET-TAG is a corresponding tag
for one or more docsets in Dash.")

(custom-autoload 'dash-at-point-mode-alist-legacy "dash-at-point" t)

(defvar dash-at-point-docsets (mapcar #'cdr dash-at-point-mode-alist) "\
Variable used to store all known Dash docsets. The default value
is a collection of all the values from `dash-at-point-mode-alist'.

Setting or appending this variable can be used to add completion
options to `dash-at-point-with-docset'.")

(defvar dash-at-point-docset nil "\
Variable used to specify the docset for the current buffer.
Users can set this to override the default guess made using
`dash-at-point-mode-alist', allowing the docset to be determined
programatically.

For example, Ruby on Rails programmers might add an \"allruby\"
tag to the Rails, Ruby and Rubygems docsets in Dash, and then add
code to `rinari-minor-mode-hook' or `ruby-on-rails-mode-hook'
which sets this variable to \"allruby\" so that Dash will search
the combined docset.")

(autoload 'dash-at-point "dash-at-point" "\
Search for the word at point in Dash.
If the optional prefix argument EDIT-SEARCH is specified,
the user will be prompted to edit the search string first.

\(fn &optional EDIT-SEARCH)" t nil)

(autoload 'dash-at-point-with-docset "dash-at-point" "\
Search for the word at point in Dash with a chosen docset.
The docset options are suggested from the variable
`dash-at-point-docsets'.

If the optional prefix argument EDIT-SEARCH is specified,
the user will be prompted to edit the search string after
choosing the docset.

\(fn &optional EDIT-SEARCH)" t nil)

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; dash-at-point-autoloads.el ends here
