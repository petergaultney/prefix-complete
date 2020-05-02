(ns autocomplete-clj.core
  (:gen-class)
  (:require [clojure.string :as str])
  (:require [clojure.pprint :as pprint]))

(defn exp [x n] ; annoying that this isn't in the core language
  (reduce * (repeat n x)))

(defn pre-suf [s n]
  [(str/join (take n s))
   (str/join (drop n s))])

(defn yield-string-prefixes [s]
  (let [max-prefix 20] ; 20 is arbitrary but usable and safe
    (mapv (partial pre-suf s) (range 1 (min max-prefix (+ 1 (count s)))))))

(defn uniq-completer-incs [namespace strings]
  (for [[s increment] (frequencies strings)
        [prefix suffix] (yield-string-prefixes s)]
    (let [namespaced-prefix (str namespace prefix)]
      {:namespaced-prefix namespaced-prefix
       :completion suffix
       :increment increment})))

(defn completer-core [completer]
  """These keys uniquely identify a completer"""
  (select-keys completer [:namespaced-prefix :completion]))

(defn make-completers [io completer-increment-seq
                       {:keys [overwrite
                               batch-size],
                        :or {overwrite false
                             batch-size (exp 10 6)}}]
  (loop [[ci-batch rest-seq] (split-at batch-size completer-increment-seq)
         completers []]
    (if (empty? ci-batch)
      completers
      (let [keyed-counts (if overwrite
                           {}
                           ((:batch-get io) (for [completer ci-batch] (completer-core completer))))]
        (recur
         (split-at batch-size rest-seq)
         (into ;; use into instead of concat because concat makes lazy nested seqs
          ;; which leads to stack overflows when it gets evaluated.
          completers
          (for [ci ci-batch]
            {:namespaced-prefix (:namespaced-prefix ci)
             :completion (:completion ci)
             :count (+  (:increment ci) (get-in keyed-counts [(completer-core ci) :count] 0))
             :length (count (:completion ci))})))))))

(defn complete-n [sorted-completions-by-prefix prefix limit]
  (take limit (get sorted-completions-by-prefix prefix #{})))

(defn compare-completers-by-key [key reverse x y]
  (let [key-comp-res (if-not reverse
                       (compare (get x key) (get y key))
                       (compare (get y key) (get x key)))]
    (if (not= key-comp-res 0)
      key-comp-res
      (compare (:completion x) (:completion y)))))

(defn get-from-map [completers-map completer-cores]
  (reduce into {}
   (remove nil?
           (for [completer-core completer-cores]
             (let [completer (get completers-map completer-core)]
               (if completer {completer-core completer} nil))))))

;; a comparer for count-ordered completers
(def compare-by-count (partial compare-completers-by-key :count true))

(defn sorted-set-by-count []
  (sorted-set-by compare-by-count))

(defn assoc-completers [{:keys [exists-map
                                sorted-sets-by-prefix
                                new-sorted-set]}
                        [completer & rest-completers]]
  (if-not completer
    ;; we've finished modifying the completion state
    {:exists-map exists-map
     :sorted-sets-by-prefix sorted-sets-by-prefix
     :new-sorted-set new-sorted-set}
    (let [existing-completer (get exists-map (completer-core completer))
          completer-prefix (:namespaced-prefix completer)
          sorted-prefix-set (get sorted-sets-by-prefix completer-prefix (new-sorted-set))]
      (recur {:exists-map (assoc exists-map (completer-core completer) completer)
              :sorted-sets-by-prefix (assoc sorted-sets-by-prefix
                                            completer-prefix
                                            (conj (disj sorted-prefix-set existing-completer)
                                                  completer))
                :new-sorted-set new-sorted-set}
             rest-completers))))

(defn completers-from-file-words [io file]
  (let [c-ns ""
        batch-size 100] ;; 100 is arbitrary but matches Dynamo limits
    (make-completers io
                     (uniq-completer-incs c-ns (str/split (slurp file) #"\s+"))
                     {:batch-size batch-size})))

(defn batch-put-core-from-file-words [io file]
  ((:batch-put io)
   (completers-from-file-words io file)))

;; global stuff for usability
(def best-completions-by-count
  (atom
   {:exists-map {}
    :sorted-sets-by-prefix {}
    :new-sorted-set sorted-set-by-count}))

;; impure - modifies singular atom for best completions by count
(defn bcbc-assoc-completers [completers]
    (swap! best-completions-by-count
           assoc-completers
           completers))

(def bcbc-io
  {:batch-get (fn [completer-cores] (get-from-map (:exists-map @best-completions-by-count) completer-cores))
   :batch-put bcbc-assoc-completers})

(defn bcbc-insert-from-file [file]
  (batch-put-core-from-file-words bcbc-io file))

(defn bcbc-complete
  ([prefix] (bcbc-complete prefix 10))
  ([prefix limit] (complete-n (:sorted-sets-by-prefix @best-completions-by-count) prefix limit)))

(def fake-io {:batch-get (fn [completer-cores] {})})

(defn -main
  [& args]
  (let [fname (nth args 0)
        to-complete (nth args 1)
        limit (Integer/parseInt (nth args 2 "10"))]
    (pprint/pprint
     (complete-n
      (:sorted-sets-by-prefix
       (assoc-completers
        {:exists-map {}
         :sorted-sets-by-prefix {}
         :new-sorted-set sorted-set-by-count}
        (completers-from-file-words fake-io fname)))
      to-complete
      limit))))
