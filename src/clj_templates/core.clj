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
(def into-identity {:to-value identity})
(extend Named IntoValue {:to-value name})
(extend String IntoValue into-identity)
(extend nil IntoValue into-identity)
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

(defn- parsed-ok? [template]
  (every? (fn [section]
            (contains? #{:expression :literals} (:type section)))
          template))

(defn- composite?
  "Whether the given value is composite i.e. can contain multiple
  values"
  [x]
  (or (sequential? x) (map? x)))

(defn- varspec-binding [varspec bindings]
  (get bindings (:name varspec)))

(defn- has-prefix? [varspec]
  (= :prefix (get-in varspec [:modifier :type])))

(defn- varspecs [template]
  (mapcat (fn [section] (get-in section [:expression :varspecs])) template))

(defn- ^{:section "2.4.1"} binding-ok? [varspec binding]
  ;;From spec: Prefix modifiers are not applicable to variables that
  ;;have composite values
  (if (has-prefix? varspec)
    (not (composite? binding))
    true))

(defn- types-ok?
  "Checks whether the varspecs within a template are all bound to valid
  values in the bindings map."
  [template bindings]
  (every? (fn [varspec]
            (binding-ok? varspec (varspec-binding varspec bindings)))
          (varspecs template)))

(defn- template-valid? [template bindings]
  (and
   (parsed-ok? template)
   (types-ok? template bindings)))

(defn expand
  "Expands a URI template string with the a map of bindings"
  [template bindings]
  (let [bindings (format-bindings bindings)
        lexbuf (lex/from-string template)
        t (parse/parse parse/uri-template lexbuf)
        cps (ex/expand-template t bindings)
        result (util/codepoints->string cps)]
    (if (template-valid? t bindings)
      result
      (throw (ex-info "Invalid URI template" {:expansion result})))))
