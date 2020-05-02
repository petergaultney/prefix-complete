(ns autocomplete-clj.core-test
  (:require [clojure.test :refer :all]
            [autocomplete-clj.core :refer :all]))

(deftest test-pre-suf
  (testing "pre-suf"
    (is (= ["pre-" "suf"] (pre-suf "pre-suf" 4)))
    (is (= ["p" "re-suf"] (pre-suf "pre-suf" 1)))
    (is (= ["" "pre-suf"] (pre-suf "pre-suf" 0)))
    (is (= ["pre-suf" ""] (pre-suf "pre-suf" 7)))))

(deftest test-compare-by-count
  (testing "compare by count"
    (is (= 1 (compare-by-count {:count 1} {:count 2})))
    (is (= 0 (compare-by-count {:count 1} {:count 1})))
    (is (= -1 (compare-by-count {:count 2} {:count 1})))))

(deftest test-yield-string-prefixes
  (testing "yield-string-prefixes"
    (is (= (yield-string-prefixes "bad")
           [["b" "ad"] ["ba" "d"] ["bad" ""]]))))

(deftest test-uniq-completer-incs
  (testing "uniq-completer-incs"
    (is (= (uniq-completer-incs "A$" ["boy" "bad" "bad"])
           [{:namespaced-prefix "A$b"
             :completion "oy"
             :increment 1}
            {:namespaced-prefix "A$bo"
             :completion "y"
             :increment 1}
            {:namespaced-prefix "A$boy"
             :completion ""
             :increment 1}
            {:namespaced-prefix "A$b"
             :completion "ad"
             :increment 2}
            {:namespaced-prefix "A$ba"
             :completion "d"
             :increment 2}
            {:namespaced-prefix "A$bad"
             :completion ""
             :increment 2}]))
    (is (= (take 2 (uniq-completer-incs "A$" ["boy" "bad" "bad"]))
           [{:namespaced-prefix "A$b"
             :completion "oy"
             :increment 1}
            {:namespaced-prefix "A$bo"
             :completion "y"
             :increment 1}]))))

(deftest test-make-completers
  (testing "make-completers"
    (is (= (make-completers "io" (uniq-completer-incs "A$" ["boy" "bad" "bad"]) {})
           [{:namespaced-prefix "A$b"
             :completion "oy"
             :length 2
             :count 1}
            {:namespaced-prefix "A$bo"
             :completion "y"
             :length 1
             :count 1}
            {:namespaced-prefix "A$boy"
             :completion ""
             :length 0
             :count 1}
            {:namespaced-prefix "A$b"
             :completion "ad"
             :length 2
             :count 2}
            {:namespaced-prefix "A$ba"
             :completion "d"
             :length 1
             :count 2}
            {:namespaced-prefix "A$bad"
             :completion ""
             :length 0
             :count 2}]))))
