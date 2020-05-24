(ns clj-templates.util)

(defprotocol ToCodepoint
  (to-codepoint [this]))

(extend-protocol ToCodepoint
  Character
  (to-codepoint [c] (int c))

  Integer
  (to-codepoint [i] i))

(defn codepoint->string [cp]
  (String. (int-array 1 [cp]) 0 1))

(defn codepoint->utf8
  "Returns the UTF-8 encoding of a codepoint"
  [cp]
  (.getBytes (codepoint->string cp)))

(defn string->codepoints [s]
  (.. s (codePoints) (toArray)))

(defn codepoints->string
  "Converts a sequences of codepoints to a string"
  [codepoints]
  (let [arr (int-array codepoints)        
        offset 0
        count (alength arr)]
    (String. arr offset count)))
