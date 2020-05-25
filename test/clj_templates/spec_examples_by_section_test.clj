(ns
 clj-templates.spec-examples-by-section-test
 (:require [clojure.test :refer :all] [clj-templates.core :as c])
 (:import [clojure.lang ExceptionInfo]))

(deftest
 path-style-parameter-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= ";who=fred" (c/expand "{;who}" variables)))
  (is (= ";half=50%25" (c/expand "{;half}" variables)))
  (is (= ";empty" (c/expand "{;empty}" variables)))
  (is (= ";hello=Hello" (c/expand "{;hello:5}" variables)))
  (is (= ";v=6;empty;who=fred" (c/expand "{;v,empty,who}" variables)))
  (is (= ";v=6;who=fred" (c/expand "{;v,bar,who}" variables)))
  (is (= ";x=1024;y=768" (c/expand "{;x,y}" variables)))
  (is (= ";x=1024;y=768;empty" (c/expand "{;x,y,empty}" variables)))
  (is (= ";x=1024;y=768" (c/expand "{;x,y,undef}" variables)))
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
    (c/expand "{;keys*}" variables)))))

(deftest
 variable-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "one,two,three" (c/expand "{count}" variables)))
  (is (= "one,two,three" (c/expand "{count*}" variables)))
  (is (= "/one,two,three" (c/expand "{/count}" variables)))
  (is (= "/one/two/three" (c/expand "{/count*}" variables)))
  (is (= ";count=one,two,three" (c/expand "{;count}" variables)))
  (is
   (=
    ";count=one;count=two;count=three"
    (c/expand "{;count*}" variables)))
  (is (= "?count=one,two,three" (c/expand "{?count}" variables)))
  (is
   (=
    "?count=one&count=two&count=three"
    (c/expand "{?count*}" variables)))
  (is
   (=
    "&count=one&count=two&count=three"
    (c/expand "{&count*}" variables)))))

(deftest
 reserved-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "value" (c/expand "{+var}" variables)))
  (is (= "/value/" (c/expand "{/var,empty}" variables)))
  (is (= "/value" (c/expand "{/var,undef}" variables)))
  (is (= "Hello%20World!" (c/expand "{+hello}" variables)))
  (is (= "50%25" (c/expand "{+half}" variables)))
  (is
   (=
    "http%3A%2F%2Fexample.com%2Fhome%2Findex"
    (c/expand "{base}index" variables)))
  (is
   (=
    "http://example.com/home/index"
    (c/expand "{+base}index" variables)))
  (is (= "OX" (c/expand "O{+empty}X" variables)))
  (is (= "OX" (c/expand "O{+undef}X" variables)))
  (is (= "/foo/bar/here" (c/expand "{+path}/here" variables)))
  (is (= "/foo/b/here" (c/expand "{+path:6}/here" variables)))
  (is (= "here?ref=/foo/bar" (c/expand "here?ref={+path}" variables)))
  (is
   (=
    "up/foo/barvalue/here"
    (c/expand "up{+path}{var}/here" variables)))
  (is
   (= "1024,Hello%20World!,768" (c/expand "{+x,hello,y}" variables)))
  (is (= "/foo/bar,1024/here" (c/expand "{+path,x}/here" variables)))
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
    (c/expand "{+keys*}" variables)))))

(deftest
 simple-string-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "value" (c/expand "{var}" variables)))
  (is (= "Hello%20World%21" (c/expand "{hello}" variables)))
  (is (= "50%25" (c/expand "{half}" variables)))
  (is (= "OX" (c/expand "O{empty}X" variables)))
  (is (= "OX" (c/expand "O{undef}X" variables)))
  (is (= "1024,768" (c/expand "{x,y}" variables)))
  (is
   (= "1024,Hello%20World%21,768" (c/expand "{x,hello,y}" variables)))
  (is (= "?1024," (c/expand "?{x,empty}" variables)))
  (is (= "?1024" (c/expand "?{x,undef}" variables)))
  (is (= "?768" (c/expand "?{undef,y}" variables)))
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
    (c/expand "{keys*}" variables)))))

(deftest
 fragment-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "#value" (c/expand "{#var}" variables)))
  (is (= "#Hello%20World!" (c/expand "{#hello}" variables)))
  (is (= "#50%25" (c/expand "{#half}" variables)))
  (is (= "foo#" (c/expand "foo{#empty}" variables)))
  (is (= "foo" (c/expand "foo{#undef}" variables)))
  (is
   (= "#1024,Hello%20World!,768" (c/expand "{#x,hello,y}" variables)))
  (is (= "#/foo/bar,1024/here" (c/expand "{#path,x}/here" variables)))
  (is (= "#/foo/b/here" (c/expand "{#path:6}/here" variables)))
  (is (= "#red,green,blue" (c/expand "{#list}" variables)))
  (is (= "#red,green,blue" (c/expand "{#list*}" variables)))
  (is
   (contains?
    #{"#semi,;,comma,,,dot,." "#semi,;,dot,.,comma,,"
      "#comma,,,semi,;,dot,." "#dot,.,comma,,,semi,;"
      "#comma,,,dot,.,semi,;" "#dot,.,semi,;,comma,,"}
    (c/expand "{#keys}" variables)))))

(deftest
 path-segment-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "/fred" (c/expand "{/who}" variables)))
  (is (= "/fred/fred" (c/expand "{/who,who}" variables)))
  (is (= "/50%25/fred" (c/expand "{/half,who}" variables)))
  (is (= "/fred/me%2Ftoo" (c/expand "{/who,dub}" variables)))
  (is (= "/value" (c/expand "{/var}" variables)))
  (is (= "/value/" (c/expand "{/var,empty}" variables)))
  (is (= "/value" (c/expand "{/var,undef}" variables)))
  (is (= "/value/1024/here" (c/expand "{/var,x}/here" variables)))
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
    (c/expand "{/keys*}" variables)))))

(deftest
 form-style-query-expansion
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "?who=fred" (c/expand "{?who}" variables)))
  (is (= "?half=50%25" (c/expand "{?half}" variables)))
  (is (= "?x=1024&y=768" (c/expand "{?x,y}" variables)))
  (is (= "?x=1024&y=768&empty=" (c/expand "{?x,y,empty}" variables)))
  (is (= "?x=1024&y=768" (c/expand "{?x,y,undef}" variables)))
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
    (c/expand "{?keys*}" variables)))))

(deftest
 label-expansion-with-dot-prefix
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= ".fred" (c/expand "{.who}" variables)))
  (is (= ".fred.fred" (c/expand "{.who,who}" variables)))
  (is (= ".50%25.fred" (c/expand "{.half,who}" variables)))
  (is (= "www.example.com" (c/expand "www{.dom*}" variables)))
  (is (= "X.value" (c/expand "X{.var}" variables)))
  (is (= "X.val" (c/expand "X{.var:3}" variables)))
  (is (= "X." (c/expand "X{.empty}" variables)))
  (is (= "X" (c/expand "X{.undef}" variables)))
  (is (= "X.red,green,blue" (c/expand "X{.list}" variables)))
  (is (= "X.red.green.blue" (c/expand "X{.list*}" variables)))
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
  (is (= "X" (c/expand "X{.empty_keys}" variables)))
  (is (= "X" (c/expand "X{.empty_keys*}" variables)))))

(deftest
 form-style-query-continuation
 (let
  [variables
   {"dom" ["example" "com"],
    "list" ["red" "green" "blue"],
    "dub" "me/too",
    "who" "fred",
    "hello" "Hello World!",
    "keys" {"semi" ";", "dot" ".", "comma" ","},
    "var" "value",
    "x" "1024",
    "path" "/foo/bar",
    "v" "6",
    "base" "http://example.com/home/",
    "count" ["one" "two" "three"],
    "half" "50%",
    "empty" "",
    "undef" nil,
    "y" "768",
    "empty_keys" []}]
  (is (= "&who=fred" (c/expand "{&who}" variables)))
  (is (= "&half=50%25" (c/expand "{&half}" variables)))
  (is (= "?fixed=yes&x=1024" (c/expand "?fixed=yes{&x}" variables)))
  (is (= "&var=val" (c/expand "{&var:3}" variables)))
  (is (= "&x=1024&y=768&empty=" (c/expand "{&x,y,empty}" variables)))
  (is (= "&x=1024&y=768" (c/expand "{&x,y,undef}" variables)))
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

