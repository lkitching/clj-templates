(ns clj-templates.parse
  (:require [clj-templates.lex :as lex]
            [clj-templates.util :as util])
  (:import [java.util ArrayList]
           [clojure.lang IFn]))

(defprotocol Parser
  "An item that should consume some prefix of the input and return a
  value representing the consumed input."
  (parse [this lex]))

(defprotocol Matcher
  "An item that can match and therefore advance the current position of
  a lex buffer without consuming (i.e. without advancing the current
  start position)."
  (try-match [this lex]
    "Attempt to match the current position of a lex buffer. If
    successful, returns true and advances the current
    position. Returns false if the match fails and in this case the
    state of the buffer should not be modified."))

(extend-protocol Matcher
  Character
  (try-match [c lexbuf]
    (lex/try-match lexbuf c))

  IFn
  (try-match [p lexbuf]
    (lex/try-match-p lexbuf p)))

(defn- parse-error [msg]
  (throw (ex-info msg {:type :parse})))

(defn try-parse [p lex]
  (try
    (parse p lex)
    (catch Exception ex
      nil)))

(defn- chars-p
  "Returns a predicate that returns true if the input codepoint matches
  any of the codepoints in the string s."
  [s]
  (let [cps (set (util/string->codepoints s))]
    (comp boolean cps)))

(defn one-of
  "Constructs a matcher which attempts to match with each of the input
  matchers. Returns true on the first successful match."
  [& ms]
  (reify Matcher
    (try-match [_this lexbuf]
      (boolean (some #(try-match % lexbuf) ms)))))

(defn star
  "Returns a matcher which tries to match the input matcher repeatedly
  until it fails. The returned matcher always succeeds."
  [m]
  (reify Matcher
    (try-match [_this lexbuf]
      (loop []
        (when (try-match m lexbuf)
          (recur)))
      true)))

(defn sequential
  "Returns a matcher which attempts to match the input with each input
  matcher in turn. Succeeds if each input matcher succeeds."
  [m & ms]
  (reify Matcher
    (try-match [_this lexbuf]
      (if (lex/end? lexbuf)
        false
        (let [p (lex/position lexbuf)]
          (loop [ms (cons m ms)]
            (if-let [m (first ms)]
              (if (try-match m lexbuf)
                (recur (rest ms))
                (do
                  (lex/set-position! lexbuf p)
                  false))
              true)))))))

(defn plus
  "Returns a matcher that attempts to match the input matcher one or
  more times."
  [m]
  (sequential m (star m)))

(defn- up-to
  "Retuns a matcher which attempts to match the input matcher between 0
  and n times."
  [n m]
  (reify Matcher
    (try-match [_this lexbuf]
      (loop [i n]
        (if (and (pos? i) (try-match m lexbuf))
          (recur (dec i))))
      true)))

(defn alpha [c]
  (or (<= 0x41 c 0x5A)
      (<= 0x61 c 0x7A)))

(defn digit [c] (<= 0x30 c 0x39))
(def hexdig (one-of digit (chars-p "abcdefABCDEF")))

(def pct-encoded (sequential \% hexdig hexdig))
(def unreserved (one-of alpha digit (chars-p "-._~")))
(def gen-delims (chars-p ":/?#[]@"))
(def sub-delims (chars-p "!$&'()*+,;="))

(def reserved (one-of gen-delims sub-delims))

(defn ucschar [c]
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

(defn iprivate [c]
  (or (<= 0xE000 c 0xF8FF)
      (<= 0xF0000 c 0xFFFFD)
      (<= 0x100000 c 0x10FFFD)))

(def literals (one-of
               (fn [c]
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
               ucschar
               iprivate
               pct-encoded))

(defn- codepoint->keyword [cp]
  (keyword (util/codepoints->string [cp])))

;;NOTE: The =,!@| operators are allowed by the grammar but are not
;;implemented. Excluding them from the grammar here means we don't
;;need to filter them in the expansion phase. This could make error
;;messages less helpful!
(def operator? (chars-p "+#./;?&"))

(defn list-of
  "Creates a parser which matches a non-empty list of values parsed by
  the parser p separated by sep. Returns a sequence of the parsed
  values."
  [sep p]
  (reify Parser    
    (parse [_this lexbuf]
      (loop [acc [(parse p lexbuf)]]        
        (if (lex/try-consume lexbuf sep)
          (recur (conj acc (parse p lexbuf)))
          acc)))))

(defn- to-digit [c] (- c 0x30))

(defn- parse-prefix-length [lexbuf]
  (if (try-match (sequential (fn [c] (<= 0x31 c 0x39)) (up-to 3 digit)) lexbuf)
    (reduce (fn [acc cp]
              (+ (* 10 acc) (to-digit cp)))
            0
            (lex/consume-all lexbuf))
    (parse-error "Expected prefix length")))

(def modifier? (chars-p ":*"))

;;TODO: Decide if 'nil' is a valid modifier or not!  other parsers
;;definitely consume some of the input so the optional combinator was
;;needed to wrap them if they did not. This one just returns nil if
;;there's no modifier at the start. If parsers just become functions
;;this might be ok?
(def modifier-level4
  (reify Parser
    (parse [_this lexbuf]      
      (let [op-cp (lex/try-consume-p lexbuf modifier?)]
        (when op-cp          
          (case (char op-cp)
            \* {:type :explode}
            \: {:type :prefix :max-length (parse-prefix-length lexbuf)}))))))

(def varchar (one-of alpha digit \_ pct-encoded))

(def varname
  (reify Parser    
    (parse [_this lexbuf]      
      (if (try-match (sequential varchar (star (one-of \. varchar))) lexbuf)
        (util/codepoints->string (lex/consume-all lexbuf))
        (parse-error "Expected varchar")))))

(def varspec
  (reify Parser    
    (parse [_this lexbuf]
      (let [vn (parse varname lexbuf)
            modifier (parse modifier-level4 lexbuf)]
        {:name vn
         :modifier modifier}))))

(def variable-list (list-of \, varspec))

(def inner-expression
  (reify Parser
    (parse [_this lexbuf]
      (let [op-cp (lex/try-consume-p lexbuf operator?)            
            op (if op-cp (codepoint->keyword op-cp))
            specs (parse variable-list lexbuf)]
        {:type :expression
         :expression {:op op :varspecs specs}}))))

(defn- scan-for
  "Consumes the remaining input up to and including the next instance of
  the given search character. Returns a sequence of the consumed
  codepoints."
  [lexbuf c]
  (let [search-cp (util/to-codepoint c)
        p (fn [cp] (not= search-cp cp))]
    ;;skip other characters
    (try-match (star p) lexbuf)
    (let [found (try-match c lexbuf)]
      ;;TODO: used match region for rest of parse?
      {:found found :codepoints (lex/consume-all lexbuf)})))

(def expression
  (reify Parser
    (parse [_this lexbuf]
      (let [start-pos (lex/position lexbuf)
            bracket-cp (lex/consume lexbuf \{)
            ;;find closing \}
            {:keys [found codepoints]} (scan-for lexbuf \})
            matched-cps (cons bracket-cp codepoints)]
        (if found
          (if (= 2 (- (lex/position lexbuf) start-pos))
            {:type :expression-error
             :reason :empty
             :codepoints matched-cps}
            (let [child-buf (lex/create codepoints 0 (dec (alength codepoints)))
                  expr (try-parse inner-expression child-buf)]
              (cond
                (nil? expr)
                {:type :expression-error
                 :codepoints matched-cps
                 :reason :invalid-expression}
                
                (lex/end? child-buf)
                expr

                :else
                {:type :expression-error
                 :codepoints matched-cps
                 :reason :invalid-expression})))
          {:type :expression-error
           :reason :unterminated
           :codepoints matched-cps})))))

(def uri-template
  (reify Parser    
    (parse [_this lexbuf]
      (loop [acc []]
        (cond
          (lex/end? lexbuf) acc
          (lex/matches? lexbuf \{) (recur (conj acc (parse expression lexbuf)))
          
          (try-match (plus literals) lexbuf)
          (let [lit-expr {:type :literals
                          :codepoints (lex/consume-all lexbuf)}]
            (recur (conj acc lit-expr)))
          
          :else
          (conj acc {:type :top-level-error
                     :codepoints (lex/consume-to-end lexbuf)}))))))
