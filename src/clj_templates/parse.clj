(ns clj-templates.parse
  (:require [clj-templates.lex :as lex]
            [clj-templates.util :as util])
  (:import [java.util ArrayList]))

(defprotocol Parser
  (can-start? [this c])
  (parse [this lex]))

(defn- parse-error [msg]
  (throw (ex-info msg {})))

(defn- fmap [f p]
  (reify Parser
    (can-start? [_this c] (can-start? p c))
    (parse [_this lex]
      (f (parse p lex)))))

(defn- pred [p name]
  (reify Parser
    (can-start? [_this c] (and (some? c)
                               (boolean (p c))))
    (parse [_this l]
      (let [c (lex/peek l)]
        (if (p c)
          [(lex/advance l)]
          (parse-error (str "Failed to match " name)))))))

(defn- str-enum [s]
  ;;TODO: set name? make optional?
  (pred (set (map int s)) "ENUM"))

(defn one-of [& ps]
  (reify Parser
    (can-start? [_this c] (boolean (some #(can-start? % c) ps)))
    (parse [_this l]
      (let [c (lex/peek l)]
        (if-let [p (some (fn [p] (when (can-start? p c) p)) ps)]
          (parse p l)
          (parse-error ":("))))))

(def alpha (pred (fn [c]
                   (or (<= 0x41 c 0x5A)
                       (<= 0x61 c 0x7A)))
                 "ALPHA"))

(def digit (pred (fn [c] (<= 0x30 c 0x39)) "DIGIT"))
(def hexdig (one-of digit (str-enum "abcdefABCDEF")))

(defn sequential [p & ps]
  (reify Parser
    (can-start? [_this c] (can-start? p c))
    (parse [_this l]
      (reduce (fn [acc p] (into acc (parse p l)))
              []
              (cons p ps)))))

(def pct-encoded (sequential (str-enum "%") hexdig hexdig))
(def unreserved (one-of alpha digit (str-enum "-._~")))
(def gen-delims (str-enum ":/?#[]@"))
(def sub-delims (str-enum "!$&'()*+,;="))
(def reserved (one-of gen-delims sub-delims))

(def ucschar (pred (fn [c]
                     (or (<= 0xA0 c 0xD7FF)
                         (<= 0xF900 c 0xFDCF)
                         (<= 0xFDF0 c 0xFFEF)
                         (<= 0x10000 c 0x1FFFD)
                         (<= 0x20000 c 0x2FFFD)
                         (<= 0x30000 c 0x3FFFD)
                         (<= 0x40000 c 0x4FFFD)
                         (<= 0x50000 c 0x5FFFD)
                         (<= 0x60000 c 0x6FFFD)
                         (<= 0x70000 c 0x7FFFD)
                         (<= 0x80000 c 0x8FFFD)
                         (<= 0x90000 c 0x9FFFD)
                         (<= 0xA0000 c 0xAFFFD)
                         (<= 0xB0000 c 0xBFFFD)
                         (<= 0xC0000 c 0xCFFFD)
                         (<= 0xD0000 c 0xDFFFD)
                         (<= 0xE1000 c 0xEFFFD)))
                   "ucschar"))

(def iprivate (pred (fn [c]
                      (or (<= 0xE000 c 0xF8FF)
                          (<= 0xF0000 c 0xFFFFD)
                          (<= 0x100000 c 0x10FFFD)))
                    "iprivate"))

(def literals (one-of
               (pred (fn [c]
                       (or (= 0x21 c)
                           (<= 0x23 c 0x24)
                           (= 0x26 c)
                           (<= 0x28 c 0x3B)
                           (= 0x3D c)
                           (<= 0x3F c 0x5B)
                           (= 0x5D c)
                           (= 0x5F c)
                           (<= 0x61 c 0x7A)
                           (= 0x7E c)))
                     "")
               ucschar
               iprivate
               pct-encoded))

(defn optional [p]
  (reify Parser
    (can-start? [_this c] (can-start? p c))
    (parse [this l]
      (if (can-start? this (lex/peek l))
        (parse p l)))))

(defn- make-operator [op-codepoints]
  (keyword (util/codepoints->string op-codepoints)))

(def operator (fmap make-operator (str-enum "+#./;?&=,!@|")))

(defn list-of [sep p]
  (reify Parser
    (can-start? [_this cp] (can-start? p cp))
    (parse [_this l]
      (loop [acc [(parse p l)]]        
        (if (lex/match? l sep)
          (recur (conj acc (parse p l)))          
          acc)))))

(def explode (fmap (constantly {:type :explode}) (str-enum "*")))
(def prefix
  (reify Parser
    (can-start? [_this c] (= (int \:) c))
    (parse [_this l]
      (lex/consume l \:)
      (let [to-digit (fn [c] (- c 0x30))
            d (lex/consume-p l (fn [c] (<= 0x31 c 0x39)) "Digit between 1 and 9")
            max-length (loop [i 3
                              len (to-digit d)]                         
                         (if (and (pos? i) (can-start? digit (lex/peek l)))                           
                           (let [d (to-digit (lex/advance l))]
                             (recur (dec i) (+ (* 10 len) d)))
                           len))]
        {:type :prefix
         :max-length max-length}))))

(def modifier-level4 (one-of prefix explode))

(def varchar (one-of alpha digit (str-enum "_") pct-encoded))
(def varname
  (reify Parser
    (can-start? [_this cp] (can-start? varchar cp))
    (parse [_this l]
      (let [following (one-of (str-enum ".") varchar)
            codepoints (loop [cps (parse varchar l)]
                         (if (can-start? following (lex/peek l))
                           (recur (into cps (parse following l)))
                           cps))]
        (util/codepoints->string codepoints)))))

(def varspec
  (reify Parser
    (can-start? [_this c] (can-start? varname nil))
    (parse [_this l]
      (let [vn (parse varname l)
            modifier (parse (optional modifier-level4) l)]
        {:name vn
         :modifier modifier}))))

(def variable-list (list-of \, varspec))

(def expression
  (reify Parser
    (can-start? [_this cp] (= (util/to-codepoint \{) cp))
    (parse [_this l]
      (lex/consume l \{)
      (let [op (parse (optional operator) l)
            specs (parse variable-list l)]
        (lex/consume l \})
        {:type :expression
         :expression {:op op :varspecs specs}}))))

(defn star [p]
  (reify Parser
    (can-start? [_this cp] (can-start? p cp))
    (parse [_this l]
      (loop [acc []]
        (if (and (not (lex/end? l))
                 (can-start? p (lex/peek l)))
          (let [r (parse p l)]
            (recur (into acc r)))
          acc)))))

(def literals-segment (fmap (fn [codepoints]
                              {:type :literals
                               :codepoints codepoints})
                            (star literals)))

(defn- consume-remaining [l]
  (loop [acc []]
    (if (lex/end? l)
      acc
      (recur (conj acc (lex/advance l))))))

(def uri-template
  (reify Parser
    (can-start? [_this cp] (or (can-start? expression cp)
                               (can-start? literals cp)))
    (parse [_this l]
      (loop [acc []]
        (cond
          (lex/end? l) acc
          (can-start? expression (lex/peek l)) (recur (conj acc (parse expression l)))
          (can-start? literals-segment (lex/peek l)) (recur (conj acc (parse literals-segment l)))
          :else (let [remaining (consume-remaining l)]
                  (conj acc {:type :top-level-error
                             :codepoints remaining})))))))
