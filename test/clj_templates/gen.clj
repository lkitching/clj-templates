(ns clj-templates.gen
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.pprint :as pp])
  (:import [java.io File FilenameFilter]))

(defn- load-spec-file [spec-file]
  (with-open [r (io/reader spec-file)]
      (json/read r)))

(defn- heading->test-name [heading]
  (as-> heading v
    (string/split v #"\s+")
    (map string/lower-case v)
    (drop-while (fn [seg] (not (re-find #"^[a-z_]" seg))) v)
    (string/join "-" v)
    (symbol v)))

(defn- gen-test-case [[template expected]]
  (cond
    (= false expected)
    `(~'is (~'thrown? ~'ExceptionInfo (~'c/expand ~template ~'variables)))

    (coll? expected)
    `(~'is (~'contains? ~(set expected) (~'c/expand ~template ~'variables)))

    (string? expected)
    `(~'is (~'= ~expected (~'c/expand ~template ~'variables)))

    :else
    (throw (ex-info "Unknown expectation type" {:expected expected}))))

(defn- gen-tests [[heading {:strs [variables testcases] :as tests}]]
  (let [test-name (heading->test-name heading)]
    `(~'deftest ~test-name
      (~'let [~'variables ~variables]
       ~@(map gen-test-case testcases)))))

(defn ns-decl [ns-name]
  `(~'ns ~ns-name
   ~'(:require [clojure.test :refer :all]
              [clj-templates.core :as c])
   ~'(:import [clojure.lang ExceptionInfo])))

(defn- spec-file->test-namespace-name [^String file-name]
  (let [idx (.indexOf file-name ".")
        base-name (if (= -1 idx) file-name (.substring file-name 0 idx))]
    (str base-name "-test")))

(defn- gen-tests-file [^File test-file]
  (let [file-name (.getName test-file)
        test-specs (load-spec-file test-file)
        ns-name (spec-file->test-namespace-name file-name)
        ns-sym (symbol (str "clj-templates." ns-name))
        test-file-name (str (string/replace ns-name "-" "_") ".clj")
        test-file (io/file "test/clj_templates" test-file-name)]
    (with-open [w (io/writer test-file)]
      (pp/pprint (ns-decl ns-sym) w)
      (doseq [ts (map gen-tests test-specs)]
        (.write w (System/lineSeparator))
        (pp/pprint ts w))
      (.write w (System/lineSeparator)))))

(def json-filename-filter
  (reify FilenameFilter
    (accept [_this _dir file-name]
      (.endsWith file-name ".json"))))

(defn gen-spec-tests []
  (let [spec-dir (io/file "spec-tests")
        spec-files (.listFiles spec-dir json-filename-filter)]
    (doseq [spec-file spec-files]
      (gen-tests-file spec-file))))
