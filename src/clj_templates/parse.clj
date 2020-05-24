(ns clj-templates.parse
  (:require [clj-templates.lex :as lex]
            [clj-templates.util :as util])
  (:import [java.util ArrayList]))

(defprotocol Parser
  (can-start? [this lex])
  (parse [this lex]))

(defn- parse-error [msg]
  (throw (ex-info msg {:type :parse})))

(defn- fmap [f p]
  (reify Parser
    (can-start? [_this c] (can-start? p c))
    (parse [_this lex]
      (f (parse p lex)))))

(defn- char-p [p]
  (reify Parser
    (can-start? [_this lex] (lex/match-p? lex p))
    (parse [_this lex] (lex/consume-p lex p ""))))

(defn try-parse [p lex]
  (let [pos (lex/position lex)]
    (try
      (parse p lex)
      (catch Exception ex
        (lex/set-position! lex pos)
        nil))))

(defn- str-enum [s]  
  (char-p (set (util/string->codepoints s))))

(defn one-of [& ps]
  (reify Parser
    (can-start? [_this lex] (boolean (some #(can-start? % lex) ps)))
    (parse [_this lex]
      (if-let [p (some (fn [p] (when (can-start? p lex) p)) ps)]
        (parse p lex)
        (parse-error "No matching parser found")))))

(def alpha (char-p (fn [c]
                     (or (<= 0x41 c 0x5A)
                         (<= 0x61 c 0x7A)))))

(def digit (char-p (fn [c] (<= 0x30 c 0x39))))
(def hexdig (one-of digit (str-enum "abcdefABCDEF")))

(defn sequential [p & ps]
  (reify Parser
    (can-start? [_this c] (can-start? p c))
    (parse [_this lexbuf]
      (reduce (fn [acc p] (conj acc (parse p lexbuf)))
              []
              (cons p ps)))))

(defn star [p]
  (reify Parser
    (can-start? [_this lexbuf] (can-start? p lexbuf))
    (parse [_this lexbuf]
      (loop [acc []]
        (if (can-start? p lexbuf)
          (recur (conj acc (parse p lexbuf)))
          acc)))))

(def pct-encoded (sequential (str-enum "%") hexdig hexdig))
(def unreserved (one-of alpha digit (str-enum "-._~")))
(def gen-delims (str-enum ":/?#[]@"))
(def sub-delims (str-enum "!$&'()*+,;="))
(def reserved (one-of gen-delims sub-delims))

(def ucschar (char-p (fn [c]
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
                           (<= 0xE1000 c 0xEFFFD)))))

(def iprivate (char-p (fn [c]
                        (or (<= 0xE000 c 0xF8FF)
                            (<= 0xF0000 c 0xFFFFD)
                            (<= 0x100000 c 0x10FFFD)))))

(def literals (one-of
               (char-p (fn [c]
                         (or (= 0x21 c)
                             (<= 0x23 c 0x24)
                             (= 0x26 c)
                             (<= 0x28 c 0x3B)
                             (= 0x3D c)
                             (<= 0x3F c 0x5B)
                             (= 0x5D c)
                             (= 0x5F c)
                             (<= 0x61 c 0x7A)
                             (= 0x7E c))))
               ucschar
               iprivate
               pct-encoded))

(defn optional [p]
  (reify Parser
    (can-start? [_this lexbuf] (can-start? p lexbuf))
    (parse [this lexbuf]
      (if (can-start? this lexbuf)
        (parse p lexbuf)))))

(defn- make-operator [op-codepoint]
  (keyword (util/codepoints->string [op-codepoint])))

(def operator (fmap make-operator (str-enum "+#./;?&=,!@|")))

(defn list-of [sep p]
  (reify Parser
    (can-start? [_this lexbuf] (can-start? p lexbuf))
    (parse [_this lexbuf]
      (loop [acc [(parse p lexbuf)]]
        (if (lex/match? lexbuf sep)
          (recur (conj acc (parse p lexbuf)))
          acc)))))

(def explode (fmap (constantly {:type :explode}) (str-enum "*")))

(def prefix
  (reify Parser
    (can-start? [_this lexbuf] (lex/matches? lexbuf \:))
    (parse [_this lexbuf]
      (lex/consume lexbuf \:)
      (let [to-digit (fn [c] (- c 0x30))
            d (lex/consume-p lexbuf (fn [c] (<= 0x31 c 0x39)) "Digit between 1 and 9")
            max-length (loop [i 3
                              len (to-digit d)]
                         (if (and (pos? i) (can-start? digit lexbuf))
                           (let [d (to-digit (lex/advance lexbuf))]
                             (recur (dec i) (+ (* 10 len) d)))
                           len))]
        {:type :prefix
         :max-length max-length}))))

(def modifier-level4 (one-of prefix explode))

(def varchar (one-of alpha digit (str-enum "_") pct-encoded))
(def varname
  (reify Parser
    (can-start? [_this lexbuf] (can-start? varchar lexbuf))
    (parse [_this lexbuf]
      (let [following (one-of (str-enum ".") varchar)
            f (parse varchar lexbuf)
            r (parse (star following) lexbuf)]
        (util/codepoints->string (cons f r))))))

(def varspec
  (reify Parser
    (can-start? [_this lexbuf] (can-start? varname lexbuf))
    (parse [_this lexbuf]
      (let [vn (parse varname lexbuf)
            modifier (parse (optional modifier-level4) lexbuf)]
        {:name vn
         :modifier modifier}))))

(def variable-list (list-of \, varspec))

(def inner-expression
  (reify Parser
    (can-start? [_this lexbuf] (or (can-start? operator lexbuf)
                                   (can-start? varspec lexbuf)))
    (parse [_this lexbuf]
      (let [op (parse (optional operator) lexbuf)
            specs (parse variable-list lexbuf)]
        {:type :expression
         :expression {:op op :varspecs specs}}))))

(defn- scan-for [lexbuf c]
  (let [search-cp (util/to-codepoint c)]
    (loop []
      (if (lex/end? lexbuf)
        false
        (let [cp (lex/advance lexbuf)]
          (if (= cp search-cp)
            true
            (recur)))))))

(def expression
  (reify Parser
    (can-start? [_this lexbuf] (lex/matches? lexbuf \{))
    (parse [_this lexbuf]
      (let [start-pos (lex/position lexbuf)]
        (lex/consume lexbuf \{)
        ;;find closing \}
        (if (scan-for lexbuf \})
          (if (= 2 (- (lex/position lexbuf) start-pos))
            {:type :expression-error
             :reason :empty
             :codepoints (util/string->codepoints "{}")}
            (let [child-buf (lex/create-child lexbuf (inc start-pos) (dec (lex/position lexbuf)))
                  expr (try-parse inner-expression child-buf)]
              (cond
                (nil? expr)
                {:type :expression-error
                 :codepoints (lex/codepoints-between lexbuf start-pos (lex/position lexbuf))}
                
                (lex/end? child-buf)
                expr

                :else
                {:type :expression-error
                 :codepoints (lex/codepoints-between lexbuf start-pos (lex/position lexbuf))
                 :reason :invalid-expression})))
          {:type :expression-error
           :reason :unterminated
           :codepoints (lex/codepoints-from lexbuf start-pos)})))))

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
    (parse [_this lexbuf]
      (loop [acc []]
        (cond
          (lex/end? lexbuf) acc
          (can-start? expression lexbuf) (recur (conj acc (parse expression lexbuf)))
          (can-start? literals-segment lexbuf) (recur (conj acc (parse literals-segment lexbuf)))
          :else (let [remaining (consume-remaining lexbuf)]
                  (conj acc {:type :top-level-error
                             :codepoints remaining})))))))
