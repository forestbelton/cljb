(ns cljb.core
  (:gen-class))

(use 'cljb.rom)
(use 'cljb.insn)
(use 'clojure.java.io)

(defn insns [& insns] (flatten insns))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (with-open [output (output-stream "rom.gb")]
    (let [bytes (program "Hey" (insns nop (jp 0x0150)))]
      (.write output bytes))))
