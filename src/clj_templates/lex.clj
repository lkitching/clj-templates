(ns clj-templates.lex
  (:require [clj-templates.util :as util]))

(defn create [a]
  {:start (atom 0)
   :current (atom 0)
   :arr a})

(defn from-string [s]
  (create (util/string->codepoints s)))

(defn position [lex]
  @(:current lex))

(defn end? [{:keys [current arr]}]
  (>= @current (alength arr)))

(defn peek
  ([{:keys [arr current] :as lex}]
   (if (not (end? lex))
     (aget arr @current)))
  ([{:keys [arr current] :as lex} offset]
   (let [p (+ offset @current)]
     (if (<= p (alength arr))
       (aget arr p)))))

(defn advance [{:keys [current] :as lex}]
  {:pre [(not (end? lex))]}
  (let [c (peek lex)]
    (swap! current inc)
    c))

(defn match? [lex c]  
  (if (= (util/to-codepoint c) (peek lex))
    (do
      (advance lex)      
      true)
    false))

(defn- lex-error [lex msg]
  (throw (ex-info (format "Lex error at index %d: %s" (position lex) msg) {})))

(defn consume [lex expected]
  (if (end? lex)
    (lex-error lex (format "Expected %s but found EOF"
                           (util/codepoint->string (util/to-codepoint expected))))
    (let [c (peek lex)
          expected (util/to-codepoint expected)]
      (if (= expected c)
        (advance lex)
        (lex-error lex (format "Expected %s but found %s"
                               (util/codepoint->string expected)
                               (util/codepoint->string c)))))))

(defn consume-p [lex p desc]
  (if (end? lex)
    (lex-error lex (format "Expected to match %s but found EOF" desc))
    (let [c (peek lex)]
      (if (p c)
        (advance lex)
        (lex-error lex (format "%s does not match %s" (util/codepoint->string c) desc))))))
