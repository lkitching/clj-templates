(ns clj-templates.parse-test
  (:require [clojure.test :refer :all]
            [clj-templates.parse :refer :all]
            [clj-templates.lex :as lex]
            [clj-templates.util :as util])
  (:import [clojure.lang ExceptionInfo]))

(deftest prefix-test
  (testing "Valid"
    (let [buf (lex/from-string ":124rest")
          result (parse prefix buf)]
      (is (= {:type :prefix :max-length 124} result))
      (is (= 4 (lex/position buf)))))

  (testing "Invalid start digit"
    (let [buf (lex/from-string ":012")]
      (is (thrown? ExceptionInfo (parse prefix buf)))))

  (testing "Excess length characters"
    (let [buf (lex/from-string ":1234567")
          result (parse prefix buf)]
      (is (= {:type :prefix :max-length 1234} result))
      (is (= 5 (lex/position buf))))))

(deftest explode-test
  (testing "Valid"
    (let [buf (lex/from-string "*}")
          result (parse explode buf)]
      (is (= {:type :explode} result))
      (is (= 1 (lex/position buf))))))

(deftest expression-test
  (testing "Empty"
    (let [buf (lex/from-string "{}")
          result (parse expression buf)]
      (is (= :expression-error (:type result)))
      (is (= "{}" (util/codepoints->string (:codepoints result))))))

  (testing "Missing closing }"
    (let [s "{foo,bar*"
          buf (lex/from-string s)
          result (parse expression buf)]
      (is (= :expression-error (:type result)))
      (is (= s (util/codepoints->string (:codepoints result))))))

  #_(testing "Unsupported operator")

  (testing "Extra characters"
    (let [s "{foo,bar:10abc}"
          buf (lex/from-string s)
          result (parse expression buf)]
      (is (= :expression-error (:type result)))
      (is (= s (util/codepoints->string (:codepoints result))))))

  (testing "Valid"
    (let [buf (lex/from-string "{?x,y*,z:123}rest")
          result (parse expression buf)]
      (= :expression (:type result)))))
