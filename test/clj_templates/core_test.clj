(ns clj-templates.core-test
  (:require [clojure.test :refer :all]
            [clj-templates.core :refer :all]))

(deftest examples-test
  (let [template "http://www.example.com/foo{?query,number}"]
    (are [bindings expected] (= expected (expand template bindings))
      {:query "mycelium" :number 100} "http://www.example.com/foo?query=mycelium&number=100"
      {:number 100} "http://www.example.com/foo?number=100"
      {} "http://www.example.com/foo")))

(deftest level-1-test
  (let [bindings {:var "value" :hello "Hello World!"}]
    (are [template expected] (= expected (expand template bindings))
      "{var}" "value"
      "{hello}" "Hello%20World%21")))

(deftest level-2-test
  (let [bindings {:var "value"
                  :hello "Hello World!"
                  :path "/foo/bar"}]
    (are [template expected] (= expected (expand template bindings))
      ;;+
      "{+var}" "value"
      "{+hello}" "Hello%20World!"
      "{+path}/here" "/foo/bar/here"
      "here?ref={+path}" "here?ref=/foo/bar"

      ;;#
      "X{#var}" "X#value"
      "X{#hello}" "X#Hello%20World!")))

(deftest level-3-test
  (let [bindings {:var "value"
                  :hello "Hello World!"
                  :empty ""
                  :path "/foo/bar"
                  :x "1024"
                  :y "768"}]
    (are [template expected] (= expected (expand template bindings))
      ;;multiple variables
      "map?{x,y}" "map?1024,768"
      "{x,hello,y}" "1024,Hello%20World%21,768"

      ;;+
      "{+x,hello,y}" "1024,Hello%20World!,768"
      "{+path,x}/here" "/foo/bar,1024/here"

      ;;#
      "{#x,hello,y}" "#1024,Hello%20World!,768"
      "{#path,x}/here" "#/foo/bar,1024/here"

      ;;.
      "X{.var}" "X.value"
      "X{.x,y}" "X.1024.768"

      ;;/
      "{/var}" "/value"
      "{/var,x}/here" "/value/1024/here"

      ;;;
      "{;x,y}" ";x=1024;y=768"
      "{;x,y,empty}" ";x=1024;y=768;empty"

      ;;?
      "{?x,y}" "?x=1024&y=768"
      "{?x,y,empty}" "?x=1024&y=768&empty="

      ;;&
      "?fixed=yes{&x}" "?fixed=yes&x=1024"
      "{&x,y,empty}" "&x=1024&y=768&empty=")))

(deftest level-4-test
  (let [bindings {:var "value"
                  :hello "Hello World!"
                  :path "/foo/bar"
                  :list ["red", "green", "blue"]
                  :keys {"semi",";" "dot","." "comma",","}}]
    (are [template expected] (= expected (expand template bindings))
      ;;string expansion with value modifiers
      "{var:3}" "val"
      "{var:30}" "value"
      "{list}" "red,green,blue"
      "{list*}" "red,green,blue"
      "{keys}" "semi,%3B,dot,.,comma,%2C"
      "{keys*}" "semi=%3B,dot=.,comma=%2C"

      ;;+
      "{+path:6}/here" "/foo/b/here"
      "{+list}" "red,green,blue"
      "{+list*}" "red,green,blue"
      "{+keys}" "semi,;,dot,.,comma,,"
      "{+keys*}" "semi=;,dot=.,comma=,"

      ;;#
      "{#path:6}/here" "#/foo/b/here"
      "{#list}" "#red,green,blue"
      "{#list*}" "#red,green,blue"
      "{#keys}" "#semi,;,dot,.,comma,,"
      "{#keys*}" "#semi=;,dot=.,comma=,"

      ;;.
      "X{.var:3}" "X.val"
      "X{.list}" "X.red,green,blue"
      "X{.list*}" "X.red.green.blue"
      "X{.keys}" "X.semi,%3B,dot,.,comma,%2C"
      "X{.keys*}" "X.semi=%3B.dot=..comma=%2C"

      ;;/
      "{/var:1,var}" "/v/value"
      "{/list}" "/red,green,blue"
      "{/list*}" "/red/green/blue"
      "{/list*,path:4}" "/red/green/blue/%2Ffoo"
      "{/keys}" "/semi,%3B,dot,.,comma,%2C"
      "{/keys*}" "/semi=%3B/dot=./comma=%2C"

      ;;;
      "{;hello:5}" ";hello=Hello"
      "{;list}" ";list=red,green,blue"
      "{;list*}" ";list=red;list=green;list=blue"
      "{;keys}" ";keys=semi,%3B,dot,.,comma,%2C"
      "{;keys*}" ";semi=%3B;dot=.;comma=%2C"

      ;;?
      "{?var:3}" "?var=val"
      "{?list}" "?list=red,green,blue"
      "{?list*}" "?list=red&list=green&list=blue"
      "{?keys}" "?keys=semi,%3B,dot,.,comma,%2C"
      "{?keys*}" "?semi=%3B&dot=.&comma=%2C"

      ;;&
      "{&var:3}" "&var=val"
      "{&list}" "&list=red,green,blue"
      "{&list*}" "&list=red&list=green&list=blue"
      "{&keys}" "&keys=semi,%3B,dot,.,comma,%2C"
      "{&keys*}" "&semi=%3B&dot=.&comma=%2C")))


