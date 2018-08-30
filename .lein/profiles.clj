{:user {:pedantic? :ranges
        :plugins [;; [lein-ancient "0.6.15"]
                  [cider/cider-nrepl "0.18.0-SNAPSHOT"]
                  [lein-kibit "0.1.6"]
                  ;; [lein-cljsbuild "1.1.0"]
                  ;; [lein-cloverage "1.0.11"]
                  ;; [lein-environ "1.1.0"]
                  [lein-bin "0.3.6"]
                  [lein-pdo "0.1.1"]
                  ;; [datawalk "0.1.4-SNAPSHOT"]
                  [lein-auto "0.1.3"]
                  [com.billpiel/sayid "0.0.16"]
                  [atroche/lein-ns-dep-graph "0.2.0-SNAPSHOT"]
                  [jonase/eastwood "0.2.9"]
                  [lein-nsorg "0.2.0"]]
        :dependencies [[pjstadig/humane-test-output "0.8.3"]
                       ;; [com.cemerick/pomegranate "0.2.2"]
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
 :repl {:plugins [[cider/cider-nrepl "0.18.0-SNAPSHOT"]
                  [figwheel-sidecar "0.5.17-SNAPSHOT"]
                  [cider/piggieback "0.3.8"]
                  [refactor-nrepl "2.4.0-SNAPSHOT"]]
        :dependencies [[figwheel-sidecar "0.5.16"]
                       [cheshire "5.8.0"]
                       [org.clojure/tools.nrepl "0.2.13"]]
        :injections [(require '[cheshire.core :as json])]}
 :env {:squiggly {:checkers                 [:eastwood :kibit]
                  :eastwood-exclude-linters [:unlimited-use]}}}