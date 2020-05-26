(ns clj-templates.expansion-test
  (:require [clojure.test :refer :all]
            [clj-templates.expansion :refer :all]))

(deftest variable-defined?-test
  (are [v expected] (= expected (variable-defined? v))
    nil false
    "" true
    "string" true
    [] false
    [nil] true
    ["a" "b" "c"] true
    {} false
    {"a" nil "b" nil} false
    {"a" "b"} true))
