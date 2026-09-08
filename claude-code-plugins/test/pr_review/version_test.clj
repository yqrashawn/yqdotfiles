(ns pr-review.version-test
  (:require [clojure.test :refer [deftest is]]
            [pr-review.version :as version]))

(deftest plugin-version-is-semver
  (is (re-matches #"\d+\.\d+\.\d+" version/plugin-version)))

(deftest plugin-version-matches-manifest
  (is (= version/plugin-version (version/manifest-version "."))
      "plugin-version and plugin.json must agree — otherwise a version bump
       updates one and `claude plugin update` silently no-ops on the other"))
