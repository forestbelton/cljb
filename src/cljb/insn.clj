(ns cljb.insn
  (:gen-class))

(use 'cljb.rom)

(defn bs
  "Convert a sequence of values to a sequence of bytes"
  [values]
  (map b values))

(defmacro is-reg8
  "Determines if a given symbol is an 8-bit register"
  [reg]
  (contains? ['a 'b 'c 'd 'e 'h 'l] reg))

(defn mask8 [x] (bit-and x 0xff))

(defn encode-xpqz
  [x p q z]
  (bit-or
    (bit-shift-left (bit-and x 0x3) 6)
    (bit-shift-left (bit-and p 0x3) 4)
    (bit-shift-left (bit-and q 0x1) 3)
    (bit-and z 0x7)))

(defn encode-xyz
  [x y z]
  (bit-or
    (bit-shift-left (bit-and x 0x3) 6)
    (bit-shift-left (bit-and y 0x7) 3)
    (bit-and z 0x7)))

(defn encode
  ([x y z] (encode-xyz x y z))
  ([x p q z] (encode-xpqz x p q z)))

(defn jp
  "Unconditional jump"
  [arg]
  [0xc3 (mask8 (bit-shift-right arg 8)) (mask8 arg)])

(defn reg8 [reg]
  (.indexOf ['b 'c 'd 'e 'h 'l 'hl 'a'] reg))

; single bit operation commands

(defmacro bit
  "Test if a bit is set in a register"
  [bit reg]
  `(vec [0xcb (encode 1 ~bit (reg8 (quote ~reg)))]))

(defmacro res
  "Reset a bit in a register"
  [bit reg]
  `(vec [0xcb (encode 2 ~bit (reg8 (quote ~reg)))]))

(defmacro set-bit
  "Set a bit in a register"
  [bit reg]
  `(vec [0xcb (encode 3 ~bit (reg8 (quote ~reg)))]))

; cpu control commands

(def ccf [0x3f])
(def scf [0x37])
(def nop [0x00])
(def halt [0x76])
(def stop [0x10 0x00])
(def di [0x31])
(def ei [0xfb])

; jump commands
(defn cc8 [cc]
  (.indexOf ['nz 'z 'nc 'c 'po 'pe 'p 'm] cc))

(defmacro ret
  ([] `(vec [0xc9]))
  ([con] `(vec [(encode 3 (cc8 (quote ~con) 0))])))

(def reti [0xd9])
(defn rst [n] (encode 3 (/ n 8) 7))
