{:user {:pedantic? :ranges
        :plugins   [[lein-ancient "0.6.15"]
                    ;; [lein-kibit "0.1.6"]
                    ;; [polylith/lein-polylith "LATEST"]
                    ;; [lein-environ "1.1.0"]

                    ;; A Leiningen plugin that executes tasks when files are modifed
                    ;; [lein-auto "0.1.3"]

                    ;; Run multiple leiningen tasks in parallel.
                    ;; lein pdo cljsbuild auto, repl
                    ;; [lein-pdo "0.1.1"]

                    #_[com.billpiel/sayid "0.0.16"]
                    [jonase/eastwood "0.3.5"]
                    [lein-nsorg "0.3.0"]]
        :dependencies [[pjstadig/humane-test-output "0.9.0"]
                       [com.cemerick/pomegranate "1.1.0"]
                       ;; [spyscope "0.1.6"]
                       ;; [compliment "0.2.2"]
                       ;; [cider/orchard "0.3.0"]
                       [slamhound "1.5.5"]]
        :injections   [(require 'pjstadig.humane-test-output)
                       ;; (require 'spyscope.core)
                       (pjstadig.humane-test-output/activate!)]
        :aliases      {"slamhound" ["run" "-m" "slam.hound"]}}
 :mirrors {#"clojars"
           {:name         "Clojar USTC"
            ;; :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"
            :url          "https://mirrors.ustc.edu.cn/clojars/"
            :repo-manager true}
           "central"
           {:name         "central aliyun"
            :url          "https://maven.aliyun.com/nexus/content/groups/public/"
            :repo-manager true}}
 :repl    {:plugins [#_[figwheel-sidecar "0.5.17-SNAPSHOT"]
                     [cider/cider-nrepl "0.21.1"]
                     [cider/piggieback "0.4.0"]
                     [refactor-nrepl "2.4.0"]]
           :dependencies [#_[figwheel-sidecar "0.5.17-SNAPSHOT"]
                          ;; [cheshire "5.8.0"]
                          [acyclic/squiggly-clojure "0.1.9-SNAPSHOT" :exclusions [org.clojure/tools.reader]]
                          #_[org.clojure/tools.nrepl "0.2.13"]]
           :injections   []}
 ;; (require '[cheshire.core :as json])]}
 :env {:squiggly {:checkers                 [:eastwood :kibit]
                  :eastwood-exclude-linters [:unlimited-use]}}}