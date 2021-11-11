(ns lib.github.pr
  (:require
   [cheshire.core :as json]
   [lib.github.core :as gh]))

(defn my-open-pr [owner repo]
  (-> (str "/repos/" owner "/" repo "/pulls")
      (gh/cget {:query-params {:state :open}})
      deref
      :body
      (json/parse-string true)))

(defn cget [owner repo num]
  (-> (str "/repos/" owner "/" repo "/pulls/" num)
      gh/cget
      deref
      :body
      (json/parse-string true)))

(defn cmerge [owner repo num opts]
  (-> (str "/repos/" owner "/" repo "/pulls/" num "/merge")
      (gh/cput opts)))

(defn creview [owner repo num opts]
  (-> (str "/repos/" owner "/" repo "/pulls/" num "/reviews")
      (gh/cput opts)))

(defn interested-pr?
  "Detect if is interested pr, merge it if `merge-directly?` is true.

  interested pr:
  1. is not draft
  2. user is me
  3. state is open
  4. is mergeable
  5. able to clean mergable
  6. I'am the owner of member of the repo "
  ([pr] (interested-pr? pr false))
  ([{:keys [user
            draft
            state
            merged
            mergeable
            mergeable_state
            commits
            number
            base
            title
            body
            head
            author_association]}
    merge-directly?]
   (let [go-merge? (and
                    (not draft)
                    (not merged)
                    (= mergeable_state "clean")
                    mergeable
                    (or (= author_association "OWNER")
                        (= author_association "MEMBER"))
                    (= state "open")
                    (= (:login user) "yqrashawn"))]
     (when (and go-merge? merge-directly?)
       (cmerge
        (get-in base [:repo :owner :login])
        (get-in base [:repo :name])
        number
        {:body {:commit_title   title
                :commit_message (or body "")
                :sha            (:sha head)
                :merge_method   (if (> commits 1) :merge :squash)}}))
     go-merge?)))

(defn bot-auto-merge-pr?
  [{:keys [user
           draft
           state
           merged
           mergeable
           mergeable_state
           commits
           number
           base
           repo
           title
           body
           head
           merge_commit_sha
           author_association]}]
  (let [go-merge?   (and
                     (not draft)
                     (not merged)
                     (= mergeable_state "clean")
                     (= commits 1)
                     (or (= author_association "OWNER")
                         (= author_association "MEMBER"))
                     (= (get-in base [:repo :owner :login]) "Conflux-Chain")
                     (= (get-in base [:repo :name])"helios")
                     (= state "open")
                     (= (:login user) "ConfluxBot"))
        approve-pr! (-> (creview
                         (get-in repo [:owner :login])
                         (get-in base [:repo :name])
                         number
                         {:body {:commit_id merge_commit_sha
                                 :event     :APPROVE}})
                        deref
                        :status
                        (= 200))
        merge-pr!   (cmerge
                     (get-in repo [:owner :login])
                     (get repo :name)
                     number
                     {:body {:commit_title   title
                             :commit_message (or body "")
                             :sha            (:sha head)
                             :merge_method   :squash}})]
    (when go-merge?
      (if (not mergeable)
        (and (approve-pr!) (merge-pr!))
        (merge-pr!)))
    go-merge?))