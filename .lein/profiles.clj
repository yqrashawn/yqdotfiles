{:portal {:dependencies [[djblue/portal "LATEST"]]
          :injections [(require '[portal.api])]}
 :fipp {:dependencies [[fipp "LATEST"]]
        :injections [(require 'fipp.clojure)]}
 :hashp {:dependencies [[hashp "LATEST"]]
         :injections [(require 'hashp.core)]}
 ;; :nrebl {:repl-options {:nrepl-middleware [nrebl.middleware/wrap-nrebl]}
 ;;         :dependencies [[rickmoynihan/nrebl.middleware "0.3.1"]
 ;;                        [org.clojure/core.async "1.3.610"]
 ;;                        [org.openjfx/javafx-fxml "15.0.1"]
 ;;                        [org.openjfx/javafx-controls "15.0.1"]
 ;;                        [org.openjfx/javafx-graphics "15.0.1"]
 ;;                        [org.openjfx/javafx-media "15.0.1"]
 ;;                        [org.openjfx/javafx-swing "15.0.1"]
 ;;                        [org.openjfx/javafx-base "15.0.1"]
 ;;                        [org.openjfx/javafx-web "15.0.1"]
 ;;                        [cljfmt "0.7.0"]]
 ;;         :resource-paths ["/Users/yqrashawn/REBL-0.9.220/REBL-0.9.220.jar"] ;; set this to where your REBL jar is installed
 ;;         :injections [(require 'nrebl.middleware) (require '[cognitect.rebl :as rebl])]}
 :reveal {:dependencies [[vlaaad/reveal "LATEST"]]
          :repl-options {:nrepl-middleware [vlaaad.reveal.nrepl/middleware]}}
 :repl {:pedantic? :ranges
        :plugins [[lein-ancient "LATEST"]
                  [refactor-nrepl "LATEST"]
                  [cider/cider-nrepl "LATEST"]
                  [clj-commons/pomegranate "LATEST"] ;; use to modify classpath at runtime

                  ;; [lein-kibit "0.1.6"]
                  ;; [lein-environ "1.1.0"]

                  ;; A Leiningen plugin that executes tasks when files are modifed
                  ;; [lein-auto "0.1.3"]

                  ;; Run multiple leiningen tasks in parallel.
                  ;; lein pdo cljsbuild auto, repl
                  ;; [lein-pdo "0.1.1"]

                  #_[com.billpiel/sayid "0.0.16"]
                  [jonase/eastwood "LATEST"]
                  [acyclic/squiggly-clojure "LATEST" :exclusions [org.clojure/tools.reader]] ;; flycheck
                  [lein-nsorg "LATEST"]]
        :dependencies [[pjstadig/humane-test-output "LATEST"]
                       [clj-commons/pomegranate "LATEST"]
                       [cider/orchard "LATEST"]]
        :injections [(require 'pjstadig.humane-test-output)
                     ;; (require 'spyscope.core)
                     (pjstadig.humane-test-output/activate!)]
        :aliases {;; "slamhound" ["run" "-m" "slam.hound"]
                  }}
 :user [;; :nrebl
        :hashp
        :fipp
        :reveal
        ;; :portal
        :repl]

 :mirrors {#"clojars"
           {:name "Clojar USTC"
            ;; :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"
            :url "https://mirrors.ustc.edu.cn/clojars/"
            :repo-manager true}
           "central"
           {:name "central aliyun"
            :url "https://maven.aliyun.com/nexus/content/groups/public/"
            :repo-manager true}
           "cognitect-dev-tools"
           {:url "https://dev-tools.cognitect.com/maven/releases/"
            :creds :gpg}}
 :env {:squiggly {:checkers [:eastwood :kibit]
                  :eastwood-exclude-linters [:unlimited-use]}}}