(ns clj-templates.core
  (:require [clj-templates.util :as util]
            [clj-templates.lex :as lex]
            [clj-templates.parse :as parse]
            [clj-templates.expansion :as ex]
            [clojure.walk :as walk])
  (:import [clojure.lang Named]
           [java.lang Number]))

(defprotocol IntoValue
  (to-value [this]))

(def into-string {:to-value str})
(extend Named IntoValue {:to-value name})
(extend String IntoValue {:to-value identity})
(extend Number IntoValue into-string)
(extend Boolean IntoValue into-string)
(extend Character IntoValue into-string)

(defn- format-value [v]
  (cond
    (map? v) (into {} (map (fn [[k v]] [(name k) (to-value v)]) v))
    (sequential? v) (into (empty v) (map to-value v))
    :else (to-value v)))

(defn format-bindings [bindings]
  (into {} (map (fn [[k v]]
                  [(name k) (format-value v)])
                bindings)))

(defn expand
  "Expands a URI template string with the a map of bindings"
  [template bindings]
  (let [bindings (format-bindings bindings)
        lexbuf (lex/from-string template)
        t (parse/parse parse/uri-template lexbuf)
        cps (ex/expand-template t bindings)]
    (util/codepoints->string cps)))

