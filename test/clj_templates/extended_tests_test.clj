(ns
 clj-templates.extended-tests-test
 (:require [clojure.test :refer :all] [clj-templates.core :as c])
 (:import [clojure.lang ExceptionInfo]))

(deftest
 additional-examples-1
 (let
  [variables
   {"random" "šöäŸœñê€£¥‡ÑÒÓÔÕÖ×ØÙÚàáâãäåæçÿ",
    "lang" "en",
    "word" "drücken",
    "token" "12345",
    "q" "URI Templates",
    "long" 37.76,
    "query"
    "PREFIX dc: <http://purl.org/dc/elements/1.1/> SELECT ?book ?who WHERE { ?book dc:creator ?who }",
    "uri" "http://example.org/?uri=http%3A%2F%2Fexample.org%2F",
    "group_id" "12345",
    "id" "person",
    "Stra%C3%9Fe" "Grüner Weg",
    "last.name" "Doe",
    "number" 6,
    "assoc_special_chars" {"šöäŸœñê€£¥‡ÑÒÓÔÕ" "Ö×ØÙÚàáâãäåæçÿ"},
    "first_name" "John",
    "lat" -122.427,
    "fields" ["id" "name" "picture"],
    "geocode" ["37.76" "-122.427"],
    "Some%20Thing" "foo",
    "page" "5",
    "format" "json"}]
  (is (= "/person" (c/expand "{/id*}" variables)))
  (is
   (contains?
    #{"/person?fields=name,picture,id&first_name=John&last.name=Doe&token=12345"
      "/person?fields=name,id,picture&first_name=John&last.name=Doe&token=12345"
      "/person?fields=picture,id,name&first_name=John&last.name=Doe&token=12345"
      "/person?fields=picture,name,id&first_name=John&last.name=Doe&token=12345"
      "/person?fields=id,picture,name&first_name=John&last.name=Doe&token=12345"
      "/person?fields=id,name,picture&first_name=John&last.name=Doe&token=12345"}
    (c/expand "{/id*}{?fields,first_name,last.name,token}" variables)))
  (is
   (contains?
    #{"/search.json?q=URI%20Templates&geocode=37.76,-122.427&lang=en&page=5"
      "/search.json?q=URI%20Templates&geocode=-122.427,37.76&lang=en&page=5"}
    (c/expand
     "/search.{format}{?q,geocode,lang,locale,page,result_type}"
     variables)))
  (is (= "/test/foo" (c/expand "/test{/Some%20Thing}" variables)))
  (is (= "/set?number=6" (c/expand "/set{?number}" variables)))
  (is
   (=
    "/loc?long=37.76&lat=-122.427"
    (c/expand "/loc{?long,lat}" variables)))
  (is
   (=
    "/base/12345/John/pages/5/en?format=json&q=URI%20Templates"
    (c/expand
     "/base{/group_id,first_name}/pages{/page,lang}{?format,q}"
     variables)))
  (is
   (=
    "/sparql?query=PREFIX%20dc%3A%20%3Chttp%3A%2F%2Fpurl.org%2Fdc%2Felements%2F1.1%2F%3E%20SELECT%20%3Fbook%20%3Fwho%20WHERE%20%7B%20%3Fbook%20dc%3Acreator%20%3Fwho%20%7D"
    (c/expand "/sparql{?query}" variables)))
  (is
   (=
    "/go?uri=http%3A%2F%2Fexample.org%2F%3Furi%3Dhttp%253A%252F%252Fexample.org%252F"
    (c/expand "/go{?uri}" variables)))
  (is
   (=
    "/service?word=dr%C3%BCcken"
    (c/expand "/service{?word}" variables)))
  (is
   (=
    "/lookup?Stra%C3%9Fe=Gr%C3%BCner%20Weg"
    (c/expand "/lookup{?Stra%C3%9Fe}" variables)))
  (is
   (=
    "%C5%A1%C3%B6%C3%A4%C5%B8%C5%93%C3%B1%C3%AA%E2%82%AC%C2%A3%C2%A5%E2%80%A1%C3%91%C3%92%C3%93%C3%94%C3%95%C3%96%C3%97%C3%98%C3%99%C3%9A%C3%A0%C3%A1%C3%A2%C3%A3%C3%A4%C3%A5%C3%A6%C3%A7%C3%BF"
    (c/expand "{random}" variables)))
  (is
   (=
    "?%C5%A1%C3%B6%C3%A4%C5%B8%C5%93%C3%B1%C3%AA%E2%82%AC%C2%A3%C2%A5%E2%80%A1%C3%91%C3%92%C3%93%C3%94%C3%95=%C3%96%C3%97%C3%98%C3%99%C3%9A%C3%A0%C3%A1%C3%A2%C3%A3%C3%A4%C3%A5%C3%A6%C3%A7%C3%BF"
    (c/expand "{?assoc_special_chars*}" variables)))))

(deftest
 additional-examples-2
 (let
  [variables
   {"lang" "en",
    "token" "12345",
    "q" "URI Templates",
    "id" ["person" "albums"],
    "start" "5",
    "fields" ["id" "name" "picture"],
    "geocode" ["37.76" "-122.427"],
    "page" "10",
    "format" "atom"}]
  (is
   (contains?
    #{"/person/albums" "/albums/person"}
    (c/expand "{/id*}" variables)))
  (is
   (contains?
    #{"/person/albums?fields=name,id,picture&token=12345"
      "/person/albums?fields=id,picture,name&token=12345"
      "/person/albums?fields=name,picture,id&token=12345"
      "/albums/person?fields=id,picture,name&token=12345"
      "/person/albums?fields=picture,id,name&token=12345"
      "/albums/person?fields=name,id,picture&token=12345"
      "/albums/person?fields=picture,name,id&token=12345"
      "/albums/person?fields=name,picture,id&token=12345"
      "/albums/person?fields=picture,id,name&token=12345"
      "/person/albums?fields=picture,name,id&token=12345"
      "/person/albums?fields=id,name,picture&token=12345"
      "/albums/person?fields=id,name,picture&token=12345"}
    (c/expand "{/id*}{?fields,token}" variables)))))

(deftest
 additional-examples-3:-empty-variables
 (let
  [variables {"empty_list" [], "empty_assoc" {}}]
  (is (contains? #{""} (c/expand "{/empty_list}" variables)))
  (is (contains? #{""} (c/expand "{/empty_list*}" variables)))
  (is (contains? #{""} (c/expand "{?empty_list}" variables)))
  (is (contains? #{""} (c/expand "{?empty_list*}" variables)))
  (is (contains? #{""} (c/expand "{?empty_assoc}" variables)))
  (is (contains? #{""} (c/expand "{?empty_assoc*}" variables)))))

(deftest
 additional-examples-4:-numeric-keys
 (let
  [variables
   {"42"
    "The Answer to the Ultimate Question of Life, the Universe, and Everything",
    "1337" ["leet" "as" "it" "can" "be"],
    "german" {"11" "elf", "12" "zwölf"}}]
  (is
   (=
    "The%20Answer%20to%20the%20Ultimate%20Question%20of%20Life%2C%20the%20Universe%2C%20and%20Everything"
    (c/expand "{42}" variables)))
  (is
   (=
    "?42=The%20Answer%20to%20the%20Ultimate%20Question%20of%20Life%2C%20the%20Universe%2C%20and%20Everything"
    (c/expand "{?42}" variables)))
  (is (= "leet,as,it,can,be" (c/expand "{1337}" variables)))
  (is
   (=
    "?1337=leet&1337=as&1337=it&1337=can&1337=be"
    (c/expand "{?1337*}" variables)))
  (is
   (contains?
    #{"?12=zw%C3%B6lf&11=elf" "?11=elf&12=zw%C3%B6lf"}
    (c/expand "{?german*}" variables)))))

(deftest
 additional-examples-5:-explode-combinations
 (let
  [variables
   {"id" "admin",
    "token" "12345",
    "tab" "overview",
    "keys" {"key1" "val1", "key2" "val2"}}]
  (is
   (contains?
    #{"?id=admin&token=12345&key2=val2&key1=val1"
      "?id=admin&token=12345&key1=val1&key2=val2"}
    (c/expand "{?id,token,keys*}" variables)))
  (is
   (contains?
    #{"/admin?token=12345&key2=val2&key1=val1"
      "/admin?token=12345&key1=val1&key2=val2"}
    (c/expand "{/id}{?token,keys*}" variables)))
  (is
   (contains?
    #{"?id=admin&token=12345&key2=val2&key1=val1"
      "?id=admin&token=12345&key1=val1&key2=val2"}
    (c/expand "{?id,token}{&keys*}" variables)))
  (is
   (contains?
    #{"/user/admin?token=12345&tab=overview&key2=val2&key1=val1"
      "/user/admin?token=12345&tab=overview&key1=val1&key2=val2"}
    (c/expand "/user{/id}{?token,tab}{&keys*}" variables)))))

