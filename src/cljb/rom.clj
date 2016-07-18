(ns cljb.rom
  (:gen-class))

(defn b
  "Converts a value to a byte"
  [value]
  (let [masked (bit-and value 0xff)]
    (byte
      (if (< masked 0x7f)
        masked
        (- masked 0xff 1)))))

(def logo
  "The bitmap of the Nintendo logo"
  [0xCE 0xED 0x66 0x66 0xCC 0x0D 0x00 0x0B 0x03 0x73 0x00 0x83 0x00 0x0C 0x00 0x0D
   0x00 0x08 0x11 0x1F 0x88 0x89 0x00 0x0E 0xDC 0xCC 0x6E 0xE6 0xDD 0xDD 0xD9 0x99
   0xBB 0xBB 0x67 0x63 0x6E 0x0E 0xEC 0xCC 0xDD 0xDC 0x99 0x9F 0xBB 0xB9 0x33 0x3E
  ])

(def entry-point
  "The entry point of the program. NOPs and then jumps to 0x150"
  [0x00 0xc3 0x01 0x50])

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn build-title
  "Formats the game title"
  [name]
  (let [len (min (.length name) 11)
        name-trunc (.substring name 0 len)
        uppercased (.toUpperCase name-trunc)
        bytes (vec (.getBytes uppercased))]
    (pad 11 bytes 0)))

(def manufacturer-code
  "The manufacturer code of this ROM"
  (vec (.getBytes "CLJB")))

(def cgb-flag
  "Determines whether a CGB is required or not"
  [0x80]) ; Supports CGB functions, but works on old gameboys also

(defn header-checksum
  "Computes the header checksum"
  [header-bytes]
  (let [f (fn [acc byte] (- acc byte 1))]
    [(reduce f (b 0) header-bytes)]))

(defn build-header
  [name]
  (flatten [
    (build-title name)
    manufacturer-code
    cgb-flag
    [0x00 0x00] ; new licensee code
    [0x00] ; sgb flag (no sgb functions)
    [0x00] ; cartridge type (rom only)
    [0x00] ; rom size (32kb)
    [0x00] ; ram size (none)
    [0x01] ; destination code (non japanese)
    [0x33] ; old licensee code
    [0x00] ; mask rom version number
    ]))

(defn program
  "Builds a ROM from a set of instructions"
  [name insns]
  (let [header (build-header name)]
    (byte-array
      (mapcat #(map b %) [
        (repeat 0x100 0x00)
        entry-point
        logo
        header
        (header-checksum header)
        [0x00 0x00] ; global checksum (unused)
        insns
      ]))))
