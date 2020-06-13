(ns clj-templates.parse-test
  (:require [clojure.test :refer :all]
            [clj-templates.parse :refer :all]
            [clj-templates.lex :as lex]
            [clj-templates.util :as util])
  (:import [clojure.lang ExceptionInfo]))

(deftest modifier-level4-test
  (testing "prefix"
    (testing "Valid"
      (let [buf (lex/from-string ":124rest")
            result (parse modifier-level4 buf)]
        (is (= {:type :prefix :max-length 124} result))
        (is (= 4 (lex/position buf)))
        (is (lex/synced? buf))))

    (testing "Invalid start digit"
      (let [buf (lex/from-string ":012")]
        (is (thrown? ExceptionInfo (parse modifier-level4 buf)))
        ;;TODO: should parsers not consume any input on failure?
        ;;no need for backtracking!
        (is (= 1 (lex/position buf)))))

    (testing "Excess length characters"
      (let [buf (lex/from-string ":1234567")
            result (parse modifier-level4 buf)]
        (is (= {:type :prefix :max-length 1234} result))
        (is (= 5 (lex/position buf)))
        (is (lex/synced? buf)))))

  (testing "explode"
    (let [buf (lex/from-string "*}")
          result (parse modifier-level4 buf)]
      (is (= {:type :explode} result))
      (is (= 1 (lex/position buf)))
      (is (lex/synced? buf))))

  (testing "none"
    (let [buf (lex/from-string "}")
          result (parse modifier-level4 buf)]
      (is (nil? result))
      (is (= 0 (lex/position buf)))
      (is (lex/synced? buf)))))

(deftest varname-test
  (testing "valid"
    (let [s "some_var.134"
          buf (lex/from-string s)
          result (parse varname buf)]
      (is (= s result))
      (is (= (.length s) (lex/position buf)))
      (is (lex/synced? buf))))

  (testing "invalid empty"
    (let [buf (lex/from-string "")]
      (is (thrown? ExceptionInfo (parse varname buf)))))
  
  (testing "invalid start character"
    (let [buf (lex/from-string ".name")]
      (is (thrown? ExceptionInfo (parse varname buf))))))

(deftest varspec-test
  (testing "no modifier"
    (let [vn "test.var4"
          buf (lex/from-string vn)
          result (parse varspec buf)]
      (is (= {:name vn :modifier nil} result))
      (is (lex/end? buf))
      (is (lex/synced? buf))))

  (testing "explode modifier"
    (let [vn "_test23"
          buf (lex/from-string (str vn "*"))
          result (parse varspec buf)]
      (is (= {:name vn :modifier {:type :explode}} result))
      (is (lex/end? buf))
      (is (lex/synced? buf))))

  (testing "prefix modifier"
    (let [vn "test%4f_d"
          buf (lex/from-string (str vn ":341"))
          result (parse varspec buf)]
      (is (= {:name vn :modifier {:type :prefix :max-length 341}} result))
      (is (lex/end? buf))
      (is (lex/synced? buf)))))

(deftest expression-test
  (testing "Empty"
    (let [buf (lex/from-string "{}")
          result (parse expression buf)]
      (is (= :expression-error (:type result)))
      (is (= "{}" (util/codepoints->string (:codepoints result))))
      (is (lex/end? buf))
      (is (lex/synced? buf))))

  (testing "Missing closing }"
    (let [s "{foo,bar*"
          buf (lex/from-string s)
          result (parse expression buf)]
      (is (= :expression-error (:type result)))
      (is (= s (util/codepoints->string (:codepoints result))))
      (is (lex/end? buf))
      (is (lex/synced? buf))))

  #_(testing "Unsupported operator")

  (testing "Extra characters"
    (let [s "{foo,bar:10abc}"
          buf (lex/from-string s)
          result (parse expression buf)]
      (is (= :expression-error (:type result)))
      (is (= s (util/codepoints->string (:codepoints result))))
      (is (lex/end? buf))
      (is (lex/synced? buf))))

  (testing "Valid"
    (let [buf (lex/from-string "{?x,y*,z:123}rest")
          result (parse expression buf)]
      (= :expression (:type result))
      (is (= 13 (lex/position buf)))
      (is (lex/synced? buf)))))

#_(deftest literals-segment-test
  (testing "To end"
    (let [s "http://literal"
          buf (lex/from-string s)
          result (parse literals-segment buf)]
      (is (= :literals (:type result)))
      (is (= (seq (util/string->codepoints s)) (seq (:codepoints result))))
      (is (= (lex/start buf) (lex/position buf)))))

  (testing "To brace"
    (let [buf (lex/from-string "literal{x,y}")
          expected-str "literal"
          expected-cps (util/string->codepoints expected-str)
          result (parse literals-segment buf)]
      (is (= :literals (:type result)))
      (is (= (seq expected-cps) (seq (:codepoints result))))
      (is (= (alength expected-cps) (lex/position buf)))))
  
  (testing "To invalid"
    (let [buf (lex/from-string "http://lit<")
          expected-str "http://lit"
          expected-cps (util/string->codepoints expected-str)
          result (parse literals-segment buf)]
      (is (= :literals (:type result)))
      (is (= (seq expected-cps) (seq (:codepoints result))))
      (is (= (alength expected-cps) (lex/position buf))))))
