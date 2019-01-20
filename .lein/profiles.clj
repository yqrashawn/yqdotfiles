{:user {:pedantic? :ranges
        :plugins [;; [lein-ancient "0.6.15"]
                  [cider/cider-nrepl "0.20.1-SNAPSHOT"]
                  [lein-kibit "0.1.6"]
                  [polylith/lein-polylith "LATEST"]
                  ;; [lein-environ "1.1.0"]
                  ;; [lein-pdo "0.1.1"]
                  ;; [lein-auto "0.1.3"]
                  #_[com.billpiel/sayid "0.0.16"]
                  [atroche/lein-ns-dep-graph "0.2.0-SNAPSHOT"]
                  [jonase/eastwood "0.2.9"]
                  [lein-nsorg "0.2.0"]]
        :dependencies [[pjstadig/humane-test-output "0.8.3"]
                       [com.cemerick/pomegranate "1.1.0"]
                       ;; [compliment "0.2.2"]
                       ;; [cider/orchard "0.3.0"]
                       [slamhound "1.5.5"]]
        :injections [(require 'pjstadig.humane-test-output)
                     (pjstadig.humane-test-output/activate!)]
        :aliases {"slamhound" ["run" "-m" "slam.hound"]}
        :mirrors {#"clojars"
                  {:name "Clojar USTC"
                   ;; :url "https://mirrors.tuna.tsinghua.edu.cn/clojars"
                   :url "https://mirrors.ustc.edu.cn/clojars/"
                   :repo-manager true}
                  "central"
                  {:name "central aliyun"
                   :url "https://maven.aliyun.com/nexus/content/groups/public/"
                   :repo-manager true}}}
 :repl {:plugins [#_[cider/cider-nrepl "0.19.0-SNAPSHOT"]
                  #_[figwheel-sidecar "0.5.17-SNAPSHOT"]
                  [cider/piggieback "0.3.9"]
                  [refactor-nrepl "2.4.0"]]
        :dependencies [#_[figwheel-sidecar "0.5.17-SNAPSHOT"]
                       #_[cheshire "5.8.0"]
                       #_[org.clojure/tools.nrepl "0.2.13"]]
        :injections [(require '[cheshire.core :as json])]}
 :env {:squiggly {:checkers                 [:eastwood :kibit]
                  :eastwood-exclude-linters [:unlimited-use]}}}