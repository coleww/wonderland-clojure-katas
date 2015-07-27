(ns alphabet-cipher.coder)

(defn map-letters
  "alphabet ciphers 2 letters, a is keychar, b is message char (does that even matter?"
  [a b]
  (let [alphabet (clojure.string/split "abcdefghijklmnopqrstuvwxyz" #"")
        a-idx (.indexOf alphabet a)
        lastphomet (take-while #(not= % b) alphabet) ;ugh garbage
        baphomet (drop-while #(not= % b) alphabet)] ;what
    (nth (concat baphomet lastphomet) a-idx)))

(defn unmap-letters
  "alphabet ciphers 2 letters, a is keychar, b is message char (does that even matter?"
  [a b]
  (let [alphabet (clojure.string/split "abcdefghijklmnopqrstuvwxyz" #"")
        lastphomet (take-while #(not= % b) alphabet) ;ugh garbage
        baphomet (drop-while #(not= % b) alphabet)
        balpha (concat baphomet lastphomet)
        b-idx (.indexOf balpha a)
        ] ;what
    (nth alphabet b-idx)))

(defn encode [keyword message]
  (let [msg-chars (clojure.string/split message #"")
        keyword-chars (take (count msg-chars) (cycle  (clojure.string/split keyword #"")))]
    (clojure.string/join (map-indexed (fn [idx item] (map-letters item (nth keyword-chars idx))) msg-chars))
    ))




(defn decode [keyword message]
  (let [msg-chars (clojure.string/split message #"")
        keyword-chars (take (count msg-chars) (cycle (clojure.string/split keyword #"")))]
    (clojure.string/join (map-indexed (fn [idx item] (unmap-letters item (nth keyword-chars idx))) msg-chars))

    ))
