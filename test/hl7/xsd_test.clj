(ns hl7.xsd-test
  (:require [hl7.xsd :as sut]
            [clojure.test :refer :all]))


(deftest test-state-machine
  (is (= (sut/forward-transitions [{:seg "a"}{:seg "b"}{:seg "c" :required true}{:seg "c"}] [])
         {"a" {:next :a}, "b" {:next :b}, "c" {:next :c}}))
  

  (sut/forward-transitions [{:seg "a"}
                            {:name "b" :segments [{:seg "x"}
                                                  {:name "z"
                                                   :required true
                                                   :segments [{:seg "xx" :required true}
                                                              {:seg "yy"}]}
                                                  {:seg "y"}]}
                            {:seg "d" :required true}
                            {:seg "e"}] [])
  )
