(ns pr-review.version
  (:require [cheshire.core :as json]))

(def plugin-version
  "Semver of this plugin, single source of truth for tests.
   Must be kept equal to .claude-plugin/plugin.json — bump both together,
   because `claude plugin update` is a no-op when the version is unchanged."
  "0.31.0")

(defn manifest-version
  "Version recorded in the plugin manifest at `plugin-root`."
  [plugin-root]
  (-> (str plugin-root "/.claude-plugin/plugin.json")
      slurp
      (json/parse-string true)
      :version))
