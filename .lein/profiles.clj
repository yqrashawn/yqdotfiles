{:portal {:dependencies [[djblue/portal "0.6.3"]]
          :injections [(require '[portal.api])
                       (portal.api/open)]}
 :nrebl {:repl-options {:nrepl-middleware [nrebl.middleware/wrap-nrebl]}
         :dependencies [[rickmoynihan/nrebl.middleware "0.3.1"]
                        [org.clojure/core.async "1.3.610"]
                        [org.openjfx/javafx-fxml "15.0.1"]
                        [org.openjfx/javafx-controls "15.0.1"]
                        [org.openjfx/javafx-graphics "15.0.1"]
                        [org.openjfx/javafx-media "15.0.1"]
                        [org.openjfx/javafx-swing "15.0.1"]
                        [org.openjfx/javafx-base "15.0.1"]
                        [org.openjfx/javafx-web "15.0.1"]
                        [cljfmt "0.7.0"]]
         :resource-paths ["/Users/yqrashawn/REBL-0.9.220/REBL-0.9.220.jar"] ;; set this to where your REBL jar is installed
         :injections [(require 'nrebl.middleware) (require '[cognitect.rebl :as rebl])]}
 :repl {:pedantic? :ranges
        :plugins [[lein-ancient "0.6.15"]
                  ;; [cider/cider-nrepl "LATEST"]
                  ;; [clj-commons/pomegranate "1.2.0"]
                  ;; [refactor-nrepl "2.4.0"]
                  ;; [lein-kibit "0.1.6"]
                  ;; [polylith/lein-polylith "LATEST"]
                  ;; [lein-environ "1.1.0"]

                  ;; A Leiningen plugin that executes tasks when files are modifed
                  ;; [lein-auto "0.1.3"]

                  ;; Run multiple leiningen tasks in parallel.
                  ;; lein pdo cljsbuild auto, repl
                  ;; [lein-pdo "0.1.1"]

                  #_[com.billpiel/sayid "0.0.16"]
                  [jonase/eastwood "0.3.11"]
                  [acyclic/squiggly-clojure "0.1.9-SNAPSHOT" :exclusions [org.clojure/tools.reader]]
                  [lein-nsorg "0.3.0"]]
        :dependencies [[pjstadig/humane-test-output "0.10.0"]
                       [com.cemerick/pomegranate "1.1.0"]
                       ;; [spyscope "0.1.6"]
                       ;; [compliment "0.2.2"]
                       ;; [cider/orchard "0.3.0"]
                       [slamhound "1.5.5"]]
        :injections [(require 'pjstadig.humane-test-output)
                     ;; (require 'spyscope.core)
                     (pjstadig.humane-test-output/activate!)]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}}
 :user [ ;; :nrebl
        :portal
        {:signing {:gpg-key "E394C5D9A8E535A6"}}
        :repl
        {:dependencies [[hashp "0.2.0"]
                        [fipp "0.6.23"]]
         :injections [(require 'hashp.core)]}]

 :mirrors {#"clojars"
           {:name "Clojar USTC"
            ;; :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"
            :url "https://mirrors.ustc.edu.cn/clojars/"
            :repo-manager true}
           "central"
           {:name "central aliyun"
            :url "https://maven.aliyun.com/nexus/content/groups/public/"
            :repo-manager true}}
 :env {:squiggly {:checkers [:eastwood :kibit]
                  :eastwood-exclude-linters [:unlimited-use]}}}