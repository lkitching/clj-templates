(ns
 clj-templates.spec-examples-test
 (:require [clojure.test :refer :all] [clj-templates.core :as c])
 (:import [clojure.lang ExceptionInfo]))

(deftest
 level-1-examples
 (let
  [variables {"var" "value", "hello" "Hello World!"}]
  (is (= "value" (c/expand "{var}" variables)))
  (is (= "Hello%20World%21" (c/expand "{hello}" variables)))))

(deftest
 level-2-examples
 (let
  [variables
   {"var" "value", "hello" "Hello World!", "path" "/foo/bar"}]
  (is (= "value" (c/expand "{+var}" variables)))
  (is (= "Hello%20World!" (c/expand "{+hello}" variables)))
  (is (= "/foo/bar/here" (c/expand "{+path}/here" variables)))
  (is (= "here?ref=/foo/bar" (c/expand "here?ref={+path}" variables)))))

(deftest
 level-3-examples
 (let
  [variables
   {"var" "value",
    "hello" "Hello World!",
    "empty" "",
    "path" "/foo/bar",
    "x" "1024",
    "y" "768"}]
  (is (= "map?1024,768" (c/expand "map?{x,y}" variables)))
  (is
   (= "1024,Hello%20World%21,768" (c/expand "{x,hello,y}" variables)))
  (is
   (= "1024,Hello%20World!,768" (c/expand "{+x,hello,y}" variables)))
  (is (= "/foo/bar,1024/here" (c/expand "{+path,x}/here" variables)))
  (is
   (= "#1024,Hello%20World!,768" (c/expand "{#x,hello,y}" variables)))
  (is (= "#/foo/bar,1024/here" (c/expand "{#path,x}/here" variables)))
  (is (= "X.value" (c/expand "X{.var}" variables)))
  (is (= "X.1024.768" (c/expand "X{.x,y}" variables)))
  (is (= "/value" (c/expand "{/var}" variables)))
  (is (= "/value/1024/here" (c/expand "{/var,x}/here" variables)))
  (is (= ";x=1024;y=768" (c/expand "{;x,y}" variables)))
  (is (= ";x=1024;y=768;empty" (c/expand "{;x,y,empty}" variables)))
  (is (= "?x=1024&y=768" (c/expand "{?x,y}" variables)))
  (is (= "?x=1024&y=768&empty=" (c/expand "{?x,y,empty}" variables)))
  (is (= "?fixed=yes&x=1024" (c/expand "?fixed=yes{&x}" variables)))
  (is (= "&x=1024&y=768&empty=" (c/expand "{&x,y,empty}" variables)))))

(deftest
 level-4-examples
 (let
  [variables
   {"var" "value",
    "hello" "Hello World!",
    "path" "/foo/bar",
    "list" ["red" "green" "blue"],
    "keys" {"semi" ";", "dot" ".", "comma" ","}}]
  (is (= "val" (c/expand "{var:3}" variables)))
  (is (= "value" (c/expand "{var:30}" variables)))
  (is (= "red,green,blue" (c/expand "{list}" variables)))
  (is (= "red,green,blue" (c/expand "{list*}" variables)))
  (is
   (contains?
    #{"comma,%2C,dot,.,semi,%3B" "dot,.,comma,%2C,semi,%3B"
      "dot,.,semi,%3B,comma,%2C" "semi,%3B,comma,%2C,dot,."
      "semi,%3B,dot,.,comma,%2C" "comma,%2C,semi,%3B,dot,."}
    (c/expand "{keys}" variables)))
  (is
   (contains?
    #{"dot=.,semi=%3B,comma=%2C" "dot=.,comma=%2C,semi=%3B"
      "semi=%3B,dot=.,comma=%2C" "semi=%3B,comma=%2C,dot=."
      "comma=%2C,semi=%3B,dot=." "comma=%2C,dot=.,semi=%3B"}
    (c/expand "{keys*}" variables)))
  (is (= "/foo/b/here" (c/expand "{+path:6}/here" variables)))
  (is (= "red,green,blue" (c/expand "{+list}" variables)))
  (is (= "red,green,blue" (c/expand "{+list*}" variables)))
  (is
   (contains?
    #{"semi,;,comma,,,dot,." "semi,;,dot,.,comma,,"
      "dot,.,comma,,,semi,;" "dot,.,semi,;,comma,,"
      "comma,,,dot,.,semi,;" "comma,,,semi,;,dot,."}
    (c/expand "{+keys}" variables)))
  (is
   (contains?
    #{"comma=,,semi=;,dot=." "dot=.,comma=,,semi=;"
      "dot=.,semi=;,comma=," "semi=;,dot=.,comma=,"
      "semi=;,comma=,,dot=." "comma=,,dot=.,semi=;"}
    (c/expand "{+keys*}" variables)))
  (is (= "#/foo/b/here" (c/expand "{#path:6}/here" variables)))
  (is (= "#red,green,blue" (c/expand "{#list}" variables)))
  (is (= "#red,green,blue" (c/expand "{#list*}" variables)))
  (is
   (contains?
    #{"#semi,;,comma,,,dot,." "#semi,;,dot,.,comma,,"
      "#comma,,,semi,;,dot,." "#dot,.,comma,,,semi,;"
      "#comma,,,dot,.,semi,;" "#dot,.,semi,;,comma,,"}
    (c/expand "{#keys}" variables)))
  (is
   (contains?
    #{"#comma=,,dot=.,semi=;" "#comma=,,semi=;,dot=."
      "#semi=;,dot=.,comma=," "#dot=.,comma=,,semi=;"
      "#dot=.,semi=;,comma=," "#semi=;,comma=,,dot=."}
    (c/expand "{#keys*}" variables)))
  (is (= "X.val" (c/expand "X{.var:3}" variables)))
  (is (= "X.red,green,blue" (c/expand "X{.list}" variables)))
  (is (= "X.red.green.blue" (c/expand "X{.list*}" variables)))
  (is
   (contains?
    #{"X.semi,%3B,comma,%2C,dot,." "X.comma,%2C,semi,%3B,dot,."
      "X.dot,.,comma,%2C,semi,%3B" "X.comma,%2C,dot,.,semi,%3B"
      "X.semi,%3B,dot,.,comma,%2C" "X.dot,.,semi,%3B,comma,%2C"}
    (c/expand "X{.keys}" variables)))
  (is (= "/v/value" (c/expand "{/var:1,var}" variables)))
  (is (= "/red,green,blue" (c/expand "{/list}" variables)))
  (is (= "/red/green/blue" (c/expand "{/list*}" variables)))
  (is
   (= "/red/green/blue/%2Ffoo" (c/expand "{/list*,path:4}" variables)))
  (is
   (contains?
    #{"/dot,.,comma,%2C,semi,%3B" "/dot,.,semi,%3B,comma,%2C"
      "/comma,%2C,semi,%3B,dot,." "/semi,%3B,dot,.,comma,%2C"
      "/comma,%2C,dot,.,semi,%3B" "/semi,%3B,comma,%2C,dot,."}
    (c/expand "{/keys}" variables)))
  (is
   (contains?
    #{"/comma=%2C/dot=./semi=%3B" "/comma=%2C/semi=%3B/dot=."
      "/dot=./comma=%2C/semi=%3B" "/dot=./semi=%3B/comma=%2C"
      "/semi=%3B/dot=./comma=%2C" "/semi=%3B/comma=%2C/dot=."}
    (c/expand "{/keys*}" variables)))
  (is (= ";hello=Hello" (c/expand "{;hello:5}" variables)))
  (is (= ";list=red,green,blue" (c/expand "{;list}" variables)))
  (is
   (=
    ";list=red;list=green;list=blue"
    (c/expand "{;list*}" variables)))
  (is
   (contains?
    #{";keys=semi,%3B,comma,%2C,dot,." ";keys=dot,.,semi,%3B,comma,%2C"
      ";keys=dot,.,comma,%2C,semi,%3B" ";keys=comma,%2C,dot,.,semi,%3B"
      ";keys=comma,%2C,semi,%3B,dot,."
      ";keys=semi,%3B,dot,.,comma,%2C"}
    (c/expand "{;keys}" variables)))
  (is
   (contains?
    #{";comma=%2C;semi=%3B;dot=." ";semi=%3B;dot=.;comma=%2C"
      ";dot=.;comma=%2C;semi=%3B" ";comma=%2C;dot=.;semi=%3B"
      ";dot=.;semi=%3B;comma=%2C" ";semi=%3B;comma=%2C;dot=."}
    (c/expand "{;keys*}" variables)))
  (is (= "?var=val" (c/expand "{?var:3}" variables)))
  (is (= "?list=red,green,blue" (c/expand "{?list}" variables)))
  (is
   (=
    "?list=red&list=green&list=blue"
    (c/expand "{?list*}" variables)))
  (is
   (contains?
    #{"?keys=dot,.,comma,%2C,semi,%3B" "?keys=dot,.,semi,%3B,comma,%2C"
      "?keys=semi,%3B,comma,%2C,dot,." "?keys=comma,%2C,semi,%3B,dot,."
      "?keys=comma,%2C,dot,.,semi,%3B"
      "?keys=semi,%3B,dot,.,comma,%2C"}
    (c/expand "{?keys}" variables)))
  (is
   (contains?
    #{"?dot=.&semi=%3B&comma=%2C" "?semi=%3B&comma=%2C&dot=."
      "?comma=%2C&semi=%3B&dot=." "?dot=.&comma=%2C&semi=%3B"
      "?semi=%3B&dot=.&comma=%2C" "?comma=%2C&dot=.&semi=%3B"}
    (c/expand "{?keys*}" variables)))
  (is (= "&var=val" (c/expand "{&var:3}" variables)))
  (is (= "&list=red,green,blue" (c/expand "{&list}" variables)))
  (is
   (=
    "&list=red&list=green&list=blue"
    (c/expand "{&list*}" variables)))
  (is
   (contains?
    #{"&keys=comma,%2C,dot,.,semi,%3B" "&keys=comma,%2C,semi,%3B,dot,."
      "&keys=dot,.,semi,%3B,comma,%2C" "&keys=semi,%3B,dot,.,comma,%2C"
      "&keys=semi,%3B,comma,%2C,dot,."
      "&keys=dot,.,comma,%2C,semi,%3B"}
    (c/expand "{&keys}" variables)))
  (is
   (contains?
    #{"&comma=%2C&semi=%3B&dot=." "&dot=.&comma=%2C&semi=%3B"
      "&comma=%2C&dot=.&semi=%3B" "&semi=%3B&dot=.&comma=%2C"
      "&semi=%3B&comma=%2C&dot=." "&dot=.&semi=%3B&comma=%2C"}
    (c/expand "{&keys*}" variables)))))

