(ns clojurecraft.util)

; Logging---------------------------------------------------------------------------
(defmacro l [& body]
  `(let [result# (~@body)]
     (println "=====================================================")
     (println result#)
     (println "=====================================================")
     result#))

(defmacro lc [& body]
  `(let [result# (~@body)]
     (println "=====================================================")
     (println result#)
     (println "-----------------------------------------------------")
     (println (class result#))
     (println "=====================================================")
     result#))


; Other ----------------------------------------------------------------------------
(defn invert [m]
  (apply assoc {} (mapcat reverse m)))

(defn replace-array-slice
  "Return a new byte-array with the given elements replaced."
  [old-arr start new-data]
  (let [len (alength new-data)
        new-arr (byte-array old-arr)]
    (dorun (map #(aset-byte new-arr (+ start %) (aget new-data %))
                (range len)))
    new-arr))

(defn replace-array-index
  "Return a new byte-array with the given byte replaced."
  [old-arr i b]
  (let [new-arr old-arr]
    (assoc new-arr i b)))

(defn sign [i]
  (if (> i 0) 1 -1))

(defn floorint [f]
  (int (Math/floor f)))

(defn any? [s]
  (seq (filter identity s)))

(defn pow [number index]
  (loop [i 1
         a 1]
    (if (> i index)
      a
      (recur
        (inc i)
        (* a number)))))

; Bytes ----------------------------------------------------------------------------
(defn byte-seq [b]
  (loop [n 0 b b s []]
    (if (< n 8)
      (recur (inc n) (bit-shift-right b 1) (conj s (bit-and b 1)))
      (reverse s))))

(defn nbyte-seq [b]
  (loop [s []
         b b]
    (if (> b 0)
      (recur
        (cons (bit-and b 1) s)
        (bit-shift-right b 1))
      (vec s))))

(defmulti true-bit-count class)

(defmethod true-bit-count Number [b]
  (true-bit-count (nbyte-seq b)))

(defmethod true-bit-count clojure.lang.PersistentVector [s]
  (loop [s s
         i 0]
    (if (= (count s) 0)
      i
      (recur
        (pop s)
        (if (= (peek s) 1)
          (inc i)
          i)))))

(defmethod true-bit-count nil [n]
  0)

(defn top [b]
  (byte (bit-shift-right (bit-and b 0xf0) 4)))

(defn bottom [b]
  (byte (bit-and b 0x0f)))

(defn to-unsigned [b]
  (bit-and b 0xff))

(defn top-4 [b]
  "Return the top four bits of a short.

  XXXX............"
  (byte (bit-shift-right (bit-and b 0xf000) 12)))

(defn mid-4 [b]
  "Return the middle four bits of a short.

  ....XXXX........"
  (byte (bit-shift-right (bit-and b 0x0f00) 8)))

(defn bottom-8 [b]
  "Return the bottom eight bits of a short.

  ........XXXXXXXX"
  (byte (bit-and b 0xff)))

