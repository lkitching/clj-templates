(ns clj-templates.lex
  (:require [clj-templates.util :as util])
  (:refer-clojure :exclude [peek])
  (:import [java.util Arrays]))

(defn create [^ints a]
  {:current (atom 0)
   :end (alength a)
   :arr a})

(defn from-string [^ints s]
  (create (util/string->codepoints s)))

(defn create-child [lex start end]
  {:current (atom start)
   :end end
   :arr (:arr lex)})

(defn codepoints-between [{:keys [^ints arr] :as lex} ^long from ^long to]
  (Arrays/copyOfRange arr from to))

(defn codepoints-from [{:keys [end] :as lex} position]
  (codepoints-between lex position end))

(defn position [lex]
  @(:current lex))

(defn set-position! [lex p]
  {:pre [(>= p 0) (< p (:end lex))]}  
  (reset! (:current lex) p)
  nil)

(defn end? [{:keys [current end]}]
  (>= @current end))

(defn peek
  ([{:keys [^ints arr current] :as lex}]
   (if (not (end? lex))
     (aget arr @current)))
  ([{:keys [^ints arr current] :as lex} offset]
   (let [p (+ offset @current)]
     (if (<= p (alength arr))
       (aget arr p)))))

(defn advance [{:keys [current] :as lex}]
  {:pre [(not (end? lex))]}
  (let [c (peek lex)]
    (swap! current inc)
    c))

;;TODO: rename!!!
(defn match? [lex c]  
  (if (= (util/to-codepoint c) (peek lex))
    (do
      (advance lex)      
      true)
    false))

(defn matches? [lex c]
  (and (not (end? lex))
       (= (util/to-codepoint c) (peek lex))))

(defn match-p? [lex p]
  (and (not (end? lex))
       (p (peek lex))))

(defn- lex-error [lex msg]
  (throw (ex-info (format "Lex error at index %d: %s" (position lex) msg) {:type :lex})))

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

;;TODO: make desc optional
(defn consume-p [lex p desc]
  (if (end? lex)
    (lex-error lex (format "Expected to match %s but found EOF" desc))
    (let [c (peek lex)]
      (if (p c)
        (advance lex)
        (lex-error lex (format "%s does not match %s" (util/codepoint->string c) desc))))))
