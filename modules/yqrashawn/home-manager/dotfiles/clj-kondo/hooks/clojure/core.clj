(ns hooks.clojure.core
  (:require
   [clj-kondo.hooks-api :as hooks]
   [clojure.string :as string]
   [hooks.common]))

;;; TODO -- seems silly to maintain different blacklists and whitelists here than we use for the deftest `^:parallel`
;;; checker... those lists live in the Clj Kondo config file
(def ^:private symbols-allowed-in-fns-not-ending-in-an-exclamation-point
  '#{;; these toucan methods might actually set global values if it's used outside of a transaction,
     ;; but since mt/with-temp runs in a transaction, so we'll ignore them in this case.
     toucan2.core/delete!
     toucan2.core/update!
     toucan2.core/insert!
     toucan2.core/insert-returning-instance!
     toucan2.core/insert-returning-instances!
     toucan2.core/insert-returning-pk!
     toucan2.core/insert-returning-pks!
     clojure.core.async/<!!
     clojure.core.async/>!!
     clojure.core.async/alts!!
     clojure.core.async/close!
     clojure.core.async/poll!
     clojure.core.memoize/memo-clear!
     clojure.core/conj!
     clojure.core/persistent!
     clojure.core/reset!
     clojure.core/swap!
     clojure.core/volatile!
     clojure.core/vreset!
     clojure.core/vswap!
     clojure.java.jdbc/execute!
     methodical.core/add-aux-method-with-unique-key!
     methodical.core/remove-aux-method-with-unique-key!
     next.jdbc/execute!

     ;; TODO: most of these symbols shouldn't be here, we should go through them and
     ;; find the functions/macros that use them and make sure their names end with !
     ;; best way to do this is try remove each of these and rely on kondo output to find places where it's used
     clojure.test/grant-collection-perms!
     clojure.test/grant-collection-perms-fn!
     clojure.test/grant-perms-fn!
     clojure.test/purge-old-entries!
     clojure.test/revoke-collection-perms!
     clojure.test/save-results!})

(defn- end-with-exclamation?
  [s]
  (string/ends-with? s "!"))

(defn- explicitly-safe? [qualified-symbol]
  (contains? symbols-allowed-in-fns-not-ending-in-an-exclamation-point qualified-symbol))

(defn- explicitly-unsafe? [config qualified-symbol]
  (contains? (get-in config [:linters :metabase/validate-deftest :parallel/unsafe]) qualified-symbol))

(defn- unsafe? [config qualified-symbol]
  (and (or (end-with-exclamation? qualified-symbol)
           (explicitly-unsafe? config qualified-symbol))
       (not (explicitly-safe? qualified-symbol))))

(defn- non-thread-safe-form-should-end-with-exclamation*
  [{[defn-or-defmacro form-name] :children, :as node} config]
  (when-not (and (:string-value form-name)
                 (end-with-exclamation? (:string-value form-name)))
    (letfn [(walk [f form]
              (f form)
              (doseq [child (:children form)]
                (walk f child)))
            (check-node [form]
              (when-let [qualified-symbol (hooks.common/node->qualified-symbol form)]
                (when (unsafe? config qualified-symbol)
                  (hooks/reg-finding!
                   (assoc (meta form-name)
                          :message (format "The name of this %s should end with `!` because it contains calls to non thread safe form `%s`. [:metabase/test-helpers-use-non-thread-safe-functions]"
                                           (:string-value defn-or-defmacro) qualified-symbol)
                          :type :metabase/test-helpers-use-non-thread-safe-functions)))))]
      (walk check-node node))
    node))

(defn non-thread-safe-form-should-end-with-exclamation
  "Used to ensure defn and defmacro in test namespace to have name ending with `!` if it's non-thread-safe.
  A function or a macro can be defined as 'not thread safe' when their funciton name ends with a `!`.

  Only used in tests to identify thread-safe/non-thread-safe test helpers. See #37126"
  [{:keys [node cljc lang config]}]
  (when (or (not cljc)
            (= lang :clj))
    (non-thread-safe-form-should-end-with-exclamation* node config))
  {:node node})

(comment
  (require '[clj-kondo.core :as clj-kondo])
  (def form (str '(defmacro a
                    [x]
                    `(fun-call x))))

  (def form "(defmacro a
           [x]
           `(some! ~x))")

  (def form "(defun f
           [x]
           (let [g! (fn [] 1)]
           (g!)))")

  (str (hooks/parse-string form))
  (hooks/sexpr (hooks/parse-string form))

  (binding [hooks/*reload* true]
    (-> form
        (with-in-str (clj-kondo/run! {:lint ["-"]}))
        :findings))

  (do (non-thread-safe-form-should-end-with-exclamation* (hooks/parse-string form)) nil))

(defn- ns-form-node->require-node [ns-form-node]
  (some (fn [node]
          (when (and (hooks/list-node? node)
                     (let [first-child (first (:children node))]
                       (and (hooks/keyword-node? first-child)
                            (= (hooks/sexpr first-child) :require))))
            node))
        (:children ns-form-node)))

(defn- lint-require-shapes [ns-form-node]
  (doseq [node (-> ns-form-node
                   ns-form-node->require-node
                   :children
                   rest)]
    (cond
      (not (hooks/vector-node? node))
      (hooks/reg-finding! (assoc (meta node)
                                 :message "All :required namespaces should be wrapped in vectors [:metabase/require-shape-checker]"
                                 :type    :metabase/require-shape-checker))

      (hooks/vector-node? (second (:children node)))
      (hooks/reg-finding! (assoc (meta node)
                                 :message "Don't use prefix forms inside :require [:metabase/require-shape-checker]"
                                 :type    :metabase/require-shape-checker)))))

(defn- require-node->namespace-symb-nodes [require-node]
  (let [[_ns & args] (:children require-node)]
    (into []
          ;; prefixed namespace forms are NOT SUPPORTED!!!!!!!!1
          (keep (fn [node]
                  (cond
                    (hooks/vector-node? node)
                    ;; propagate the metadata attached to this vector in case there's a `:clj-kondo/ignore` form.
                    (vary-meta (first (:children node)) (partial merge (meta require-node) (meta node)))

                    ;; this should also be dead code since we require requires to be vectors
                    (hooks/token-node? node)
                    (vary-meta node (partial merge (meta require-node)))

                    :else
                    (printf "Don't know how to figure out what namespace is being required in %s\n" (pr-str node)))))
          args)))

(defn- ns-form-node->ns-symb [ns-form-node]
  (some-> (some (fn [node]
                  (when (and (hooks/token-node? node)
                             (not= (hooks/sexpr node) 'ns))
                    node))
                (:children ns-form-node))
          hooks/sexpr))

(defn- module
  "E.g.

    (module 'metabase.qp.middleware.wow) => 'metabase.qp"
  [ns-symb]
  (some-> (re-find #"^metabase\.[^.]+" (str ns-symb)) symbol))

(defn- ignored-namespace? [ns-symb config]
  (some
   (fn [pattern-str]
     (re-find (re-pattern pattern-str) (str ns-symb)))
   (:ignored-namespace-patterns config)))

(defn- module-api-namespaces
  "Set API namespaces for a given module. `:any` means you can use anything, there are no API namespaces for this
  module (yet). If unspecified, the default is just the namespace with the same name as the module e.g.
  `metabase.db`."
  [module config]
  (let [module-config (get-in config [:api-namespaces module])]
    (cond
      (= module-config :any)
      nil

      (set? module-config)
      module-config

      :else
      #{module})))

(defn- lint-modules [ns-form-node config]
  (let [ns-symb (ns-form-node->ns-symb ns-form-node)]
    (when-not (ignored-namespace? ns-symb config)
      (when-let [current-module (module ns-symb)]
        (let [allowed-modules               (get-in config [:allowed-modules current-module])
              required-namespace-symb-nodes (-> ns-form-node
                                                ns-form-node->require-node
                                                require-node->namespace-symb-nodes)]
          (doseq [node  required-namespace-symb-nodes
                  :let  [clj-kondo-ignore (some-> (meta node) :clj-kondo/ignore hooks/sexpr set)]
                  :when (not (contains? clj-kondo-ignore :metabase/ns-module-checker))
                  :let  [required-namespace (hooks/sexpr node)
                         required-module    (module required-namespace)]
                  ;; ignore stuff not in a module i.e. non-Metabase stuff.
                  :when required-module
                  :let  [in-current-module? (= required-module current-module)]
                  :when (not in-current-module?)
                  :let  [allowed-module?           (or (= allowed-modules :any)
                                                       (contains? (set allowed-modules) required-module))
                         module-api-namespaces     (module-api-namespaces required-module config)
                         allowed-module-namespace? (or (empty? module-api-namespaces)
                                                       (contains? module-api-namespaces required-namespace))]]
            (when-let [error (cond
                               (not allowed-module?)
                               (format "Module %s should not be used in the %s module. [:metabase/ns-module-checker :allowed-modules %s]"
                                       required-module
                                       current-module
                                       current-module)

                               (not allowed-module-namespace?)
                               (format "Namespace %s is not an allowed external API namespace for the %s module. [:metabase/ns-module-checker :api-namespaces %s]"
                                       required-namespace
                                       required-module
                                       required-module))]
              (hooks/reg-finding! (assoc (meta node)
                                         :message error
                                         :type    :metabase/ns-module-checker)))))))))

(defn lint-ns [x]
  (lint-require-shapes (:node x))
  (lint-modules (:node x) (get-in x [:config :linters :metabase/ns-module-checker]))
  x)
