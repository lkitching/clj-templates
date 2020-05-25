(ns
 clj-templates.negative-tests-test
 (:require [clojure.test :refer :all] [clj-templates.core :as c])
 (:import [clojure.lang ExceptionInfo]))

(deftest
 failure-tests
 (let
  [variables
   {"example" "red",
    "list" ["red" "green" "blue"],
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "trailing_space " "Bye!",
    "var" "value",
    "query"
    "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?book ?who WHERE { ?book dc:creator ?who }",
    "x" "1024",
    "path" "/foo/bar",
    "~thing" "some-user",
    "id" "thing",
    "searchTerms" "uri templates",
    " leading_space" "Hi!",
    "empty" "",
    "y" "768",
    "default-graph-uri"
    ["http://www.example/book/" "http://www.example/papers/"],
    "with space" "fail"}]
  (is (thrown? ExceptionInfo (c/expand "{/id*" variables)))
  (is (thrown? ExceptionInfo (c/expand "/id*}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{/?id}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{var:prefix}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{hello:2*}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{??hello}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{!hello}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{with space}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{ leading_space}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{trailing_space }" variables)))
  (is (thrown? ExceptionInfo (c/expand "{=path}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{$var}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{|var*}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{*keys?}" variables)))
  (is
   (thrown? ExceptionInfo (c/expand "{?empty=default,var}" variables)))
  (is
   (thrown?
    ExceptionInfo
    (c/expand "{var}{-prefix|/-/|var}" variables)))
  (is
   (thrown?
    ExceptionInfo
    (c/expand "?q={searchTerms}&amp;c={example:color?}" variables)))
  (is
   (thrown? ExceptionInfo (c/expand "x{?empty|foo=none}" variables)))
  (is (thrown? ExceptionInfo (c/expand "/h{#hello+}" variables)))
  (is (thrown? ExceptionInfo (c/expand "/h#{hello+}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{keys:1}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{+keys:1}" variables)))
  (is (thrown? ExceptionInfo (c/expand "{;keys:1*}" variables)))
  (is
   (thrown? ExceptionInfo (c/expand "?{-join|&|var,list}" variables)))
  (is (thrown? ExceptionInfo (c/expand "/people/{~thing}" variables)))
  (is
   (thrown? ExceptionInfo (c/expand "/{default-graph-uri}" variables)))
  (is
   (thrown?
    ExceptionInfo
    (c/expand "/sparql{?query,default-graph-uri}" variables)))
  (is
   (thrown?
    ExceptionInfo
    (c/expand "/sparql{?query){&default-graph-uri*}" variables)))
  (is
   (thrown? ExceptionInfo (c/expand "/resolution{?x, y}" variables)))))

