(ns clj-templates.lex
  (:require [clj-templates.util :as util])
  (:refer-clojure :exclude [peek])
  (:import [java.util Arrays]))

(defn create
  "Creates a new buffer around an array of codepoints. The start and end
  position can be specified to create a specified range within the
  buffer."
  ([^ints a] (create a 0 (alength a)))
  ([^ints a start end]
   {:start (atom start)
    :current (atom start)
    :end end
    :arr a}))

(defn from-string
  "Creates a new buffer for the codepoints of a string"
  [s]
  (create (util/string->codepoints s)))

(defn position
  "The current position in the buffer"
  [lex]
  @(:current lex))

(defn start
  "The current start position in the buffer"
  [lex]
  @(:start lex))

(defn set-position! [lex p]
  {:pre [(>= p 0) (<= p (:end lex))]}  
  (reset! (:current lex) p)
  nil)

(defn end?
  "Whether the end of this buffer has been reached"
  [{:keys [current end]}]
  (>= @current end))

(defn- lex-error
  "Raises an error for the current state of the buffer"
  [lex msg]
  (throw (ex-info (format "Lex error at index %d: %s" (position lex) msg) {:type :lex})))

(defn- peek
  "Returns the codepoint at the current buffer position"
  [{:keys [^ints arr current] :as lex}]
  (if (not (end? lex))
    (aget arr @current)))

(defn- advance
  "Advances the current pointer and returns the codepoint previously at
  the current position"
  [{:keys [current] :as lex}]
  {:pre [(not (end? lex))]}
  (let [c (peek lex)]
    (swap! current inc)
    c))

(defn synced?
  "Whether the extent of the buffer is currently empty"
  [{:keys [current start] :as lex}]
  (= @current @start))

;;lex -> cp -> bool
(defn try-match
  "Advances the current position if the current codepoint matches the
  given value. Returns whether the match was successful."
  [{:keys [current] :as lex} c]
  (if (= (util/to-codepoint c) (peek lex))
    (do
      (swap! current inc)
      true)
    false))

;;lex -> (cp -> bool) -> bool
(defn try-match-p
  "Advances current and returns the matched character if it matches the
  specified predicate. Returns a falsey value and leaves current
  unchanged if the current character does not pass the predicate."
  [lex p]
  (if (end? lex)
    false
    (let [c (peek lex)]
      (if (p c)
        (advance lex)
        false))))

(defn- consume!
  "Advances the current position and advances the start pointer to the
  new position"
  [{:keys [start current] :as lex}]
  (swap! current inc)
  (reset! start @current)
  nil)

;;lex -> cp -> bool
(defn try-consume
  "Advances the current and start pointers if the current codepoint
  matches the specified value. Returns whether the match was
  successful"
  [lex c]
  (if (= (util/to-codepoint c) (peek lex))
    (do
      (consume! lex)
      true)
    false))

;;lex -> (cp -> bool) -> (bool | cp)
(defn try-consume-p
  "Advances the current and start pointers if the current codepoint
  satisfies the given predicate. Returns the consumed codepoint if
  successful or a falsey value if the match failed."
  [lex p]
  (if (end? lex)
    false
    (let [c (peek lex)]
      (if (p c)
        (do
          (consume! lex)
          c)
        false))))

;;lex -> cp -> bool
(defn matches?
  "Returns whether the current codepoint matches the specified value
  without advancing."
  [lex c]
  (and (not (end? lex))
       (= (util/to-codepoint c) (peek lex))))

;;lex -> cp -> cp
(defn consume
  "Advances the current and start pointers passed the codepoint at the
  current position. If a codepoint is specified an exception will be
  raised if the current codepoint does not match the expected value."
  ([{:keys [start current] :as lex}]
   (if (end? lex)
     (lex-error lex "Cannot consume: EOF")
     (let [c (advance lex)]
       (reset! start @current)
       c)))
  ([lex expected]
   (if (end? lex)
     (lex-error lex (format "Expected %s but found EOF"
                            (util/codepoint->string (util/to-codepoint expected))))
     (let [c (peek lex)
           expected (util/to-codepoint expected)]
       (if (= expected c)
         (consume lex)
         (lex-error lex (format "Expected %s but found %s"
                                (util/codepoint->string expected)
                                (util/codepoint->string c))))))))

;;lex -> [cp]
(defn consume-all
  "Advances the start position to the current position and returns the
  array of codepoints between the old and new start positions."
  [{:keys [arr start current]}]
  (let [^long c @current
        cps (Arrays/copyOfRange arr @start c)]
    (reset! start c)
    cps))

(defn consume-to-end
  "Advances to the end of the buffer and returns an array of the
  codepoints from the old start position."
  [{:keys [arr current] :as lex}]
  (if (end? lex)
    (int-array 0)
    (do
      (set-position! lex (alength arr))
      (consume-all lex))))
