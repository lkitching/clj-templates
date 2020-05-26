(ns clj-templates.expansion
  (:require [clj-templates.lex :as lex]
            [clj-templates.parse :as p]
            [clj-templates.util :as util])
  (:import [java.nio.charset Charset]))

(def utf-8-enc (Charset/forName "utf8"))

(defn- pct-encode
  "Returns a sequences of codepoints representing the percent-encoding of a byte"
  [^Byte b]
  (let [hex-chars "0123456789ABCDEF"
        lower (bit-and b 0xf)
        upper (bit-shift-right (bit-and 0xff b) 4)
        cs [\% (.charAt hex-chars upper) (.charAt hex-chars lower)]]
    (map util/to-codepoint cs)))

(defn- pct-encode-codepoint
  "Returns a sequence of codepoints representing the percent-encoding of a codepoint"
  [cp]
  (mapcat pct-encode (util/codepoint->utf8 cp)))

(def allow-parsers {:u p/unreserved :r p/reserved})

(def operators
  {nil {:first "" :sep "," :named false :ifemp "" :allow #{:u}}
   :+  {:first "" :sep "," :named false :ifemp "" :allow #{:u :r}}
   :.  {:first "." :sep "." :named false :ifemp "" :allow #{:u}}
   :/  {:first "/" :sep "/" :named false :ifemp "" :allow #{:u}}
   (keyword ";") {:first ";" :sep ";" :named true :ifemp "" :allow #{:u}}
   :? {:first "?" :sep "&" :named true :ifemp "=" :allow #{:u}}
   :& {:first "&" :sep "&" :named true :ifemp "=" :allow #{:u}}
   :# {:first "#" :sep "," :named false :ifemp "" :allow #{:u :r}}})

(defmulti expand (fn [bindings value] (:type value)))

(defmethod expand :literals [_bindings {:keys [codepoints]}]  
  codepoints)

(defn- encode-literal [s]
  (let [lexbuf (lex/from-string s)]
    (loop [acc []]
      (if (lex/end? lexbuf)
        acc
        (cond
          ;;codepoint is in reservered / unreserved
          (or (p/can-start? p/unreserved lexbuf) (p/can-start? p/reserved lexbuf))
          (recur (conj acc (lex/advance lexbuf)))

          ;;see if start of percent-encoded triple
          (p/can-start? p/pct-encoded lexbuf)
          (if-let [pct (p/try-parse p/pct-encoded lexbuf)]
            (recur (into acc pct))
            (let [cp (lex/advance lexbuf)]
              (recur (into acc (pct-encode-codepoint cp)))))           

          :else
          (recur (into acc (pct-encode-codepoint (lex/advance lexbuf)))))))))

(defn- encode-varname [var-name]
  (encode-literal var-name))

(defn- allow->parser [allow]
  (apply p/one-of (map allow-parsers allow)))

;;TODO: refactor!
(defn- encode-codepoints [codepoints allow]
  (let [ap (allow->parser allow)
        lexbuf (lex/create (int-array codepoints))]
    (loop [acc []]
      (cond
        (lex/end? lexbuf)
        acc
        
        (p/can-start? ap lexbuf)
        (recur (conj acc (lex/advance lexbuf)))

        :else
        (recur (into acc (pct-encode-codepoint (lex/advance lexbuf))))))))

(defn- encode-string [s prefix allow]
  (let [codepoints (util/string->codepoints s)        
        to-encode (if (nil? prefix)
                    codepoints
                    (take prefix codepoints))]
    (encode-codepoints to-encode allow)))

(defn- expand-string [s varspec {:keys [named ifemp allow] :as op-spec}]
  (let [max-length (get-in varspec [:modifier :max-length])]
    (cond
      (and named (empty? s))
      (concat (encode-varname (:name varspec)) (util/string->codepoints ifemp))

      named
      (let [name-bit (concat (encode-varname (:name varspec)) (util/string->codepoints "="))]
        (concat name-bit (encode-string s max-length allow)))

      :else
      (encode-string s max-length allow))))

(defn- defined? [x]
  (some? x))

(defn- defined-pair? [[k v]]
  ;;NOTE: spec says 'append each pair with a defined value'
  (defined? v))

(defn- separated [vs sep]
  (if (seq vs)
    (let [sep-codepoints (util/string->codepoints sep)]
      (concat (first vs) (mapcat (fn [v] (concat sep-codepoints v)) (rest vs))))))

(defn- encode-sequential [s sep allow]
  ;;TODO: check whether values should be encoded separately from the separator
  ;;spec says:
  ;;append each defined list member to the result string, after pct-encoding any characters that are
  ;;not in the allow set, with the sep string appended to the
  ;;result between each defined list member.
  (let [encoded-values (keep (fn [v]
                               (when (defined? v)
                                 (encode-string v nil allow)))
                             s)]
    (separated encoded-values sep)))

(defn- encode-associative [v inter sep allow]
  (let [pairs (filter defined-pair? v)
        inter-codepoints (util/string->codepoints inter)
        ps (map (fn [[k v]] (concat (encode-string k nil allow)
                                    inter-codepoints
                                    (encode-string v nil allow))) pairs)]
    (separated ps sep)))

(defn- expand-compound-no-explode [v varspec {:keys [named ifemp allow] :as op-spec}]  
  (if (and named (empty? v))
    (concat (encode-varname (:name varspec)) (util/string->codepoints ifemp))
    (let [name-prefix (if named
                        (concat (encode-varname (:name varspec)) (util/string->codepoints "=")))
          value-str (if (sequential? v)
                      (encode-sequential v "," allow)
                      (encode-associative v "," "," allow))]
      (concat name-prefix value-str))))

(defn- expand-compound-explode [v varspec {:keys [named sep ifemp allow] :as op-spec}]
  (if named
    (let [pairs (if (sequential? v)
                  (map vector (repeat (:name varspec)) (filter defined? v))
                  (filter defined-pair? v))
          pair-strs (map (fn [[k v]]
                           (concat (encode-literal k)
                                   (if (defined? v)
                                     (concat (util/string->codepoints "=") (encode-string v nil allow))
                                     (util/string->codepoints ifemp))))
                         pairs)]
      (separated pair-strs sep))
    (if (sequential? v)
      (encode-sequential v sep allow)
      (encode-associative v "=" sep allow))))

(defn- expand-var [{:keys [value varspec first?]} {:keys [named] :as op-spec}]
  (let [modifier-type (get-in varspec [:modifier :type])
        explode? (= :explode modifier-type)
        value-str (cond
                    (string? value)
                    (expand-string value varspec op-spec)

                    (not explode?)
                    (expand-compound-no-explode value varspec op-spec)

                    :else
                    (expand-compound-explode value varspec op-spec))
        first-str (if first? (:first op-spec) (:sep op-spec))]
    (concat (util/string->codepoints first-str)
            value-str)))

;;TODO: normalise variables by removing undefined sub-elements from lists/maps?
;;expansion process for compound variables has to scan for undefined values
(defn ^{:section "2.3"} variable-defined? [v]
  (cond (nil? v) false
        (string? v) true
        ;;TODO: are lists containing only undefined values undefined?
        (sequential? v) (boolean (seq v)) ;;undefined if zero members
        :else (boolean (some defined-pair? v)))) ;;undefined if all values are undefined

(defmethod expand :expression [bindings {:keys [expression]}]
  (let [{:keys [op varspecs]} expression
        op-spec (get operators op)
        defined (keep (fn [varspec]
                        (let [value (get bindings (:name varspec))]
                          (if (variable-defined? value)
                            {:value value :varspec varspec})))
                      varspecs)
        var-bindings (map-indexed (fn [idx d] (assoc d :first? (zero? idx))) defined)]
    (mapcat (fn [vb] (expand-var vb op-spec)) var-bindings)))

(defmethod expand :expression-error [_bindings {:keys [codepoints]}]
  codepoints)

(defmethod expand :top-level-error [_bindings {:keys [codepoints]}]
  codepoints)

(defn expand-template [parsed-template bindings]
  (mapcat (fn [c] (expand bindings c)) parsed-template))
