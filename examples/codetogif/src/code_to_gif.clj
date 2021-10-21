(ns code-to-gif
  (:require [membrane.ui :as ui]
            [membrane.components.code-editor.code-editor :as code-editor]
            [liq.buffer :as buffer]
            [liq.highlighter :as highlighter]
            [membrane.skia :as skia]
            [avclj :as avclj]
            [avclj.av-codec-ids :as codec-ids]
            clojure.pprint
            [com.phronemophobic.clj-media.skia :as clj-media]
            ;; [tech.v3.tensor :as dtt]
            ;; [tech.v3.datatype.ffi :as dt-ffi]
            ;; [tech.v3.datatype :as dtype]
            ;; [tech.v3.libs.buffered-image :as bufimg]
            )
  (:gen-class))


(defn- insert-text [buf s]
  (-> buf
      (buffer/insert-string )
      (highlighter/highlight code-editor/hl)
      ))

(defn- buf-view [buf]
  (code-editor/->Buffer nil true
                        buf))

(defn code->views [code-str]
  (->> (reductions (fn [{:keys [buf]} c]
                     {:buf (-> buf
                               (buffer/insert-char c)
                               (highlighter/highlight code-editor/hl))
                      :c c})
                   {:buf (buffer/buffer "" {})}
                   code-str)
       (remove (fn [{:keys [c]}]
                 (= c \space)))
       (map :buf)
       (map buf-view)
       (map #(ui/padding 4 %))))

(defn make-code-gif
  "Write a gif to `fname` that will animate printing `code-str`"
  [fname code-str]
  (let [views (code->views code-str)
        bounds (map ui/bounds views)
        maxx (->> bounds
                  (map first)
                  (apply max))
        maxy (->> bounds
                  (map second)
                  (apply max))]
    (clj-media/write-gif
     fname
     (concat views
             (repeat (* 48 5) (last views)))
     maxx maxy)))


(defn repl-to-gif
  "Write a gif to `fname` that will create an animated gif that looks like a repl.

  `fname`: A string path to write the gif to.
  `statements`: A sequence of strings of source code. 
  Each string will be `read-string`'d and `eval`'d. 
  The result will be pretty printed in the gif.

  Example:
  (repl-to-gif \"repl.gif\"
               [\"\\\"Hello World!\\\"\"
                \"(repeat 7 \\\"I am using the REPL! 💪\\\")\"
                \"(map (fn [s]
         (if (< (count s) 5)
           (str \\\"Give me \\\" s \\\"! ~•~ \\\" (last s) \\\"!\\\")
           s))
       [\\\"an R\\\" \\\"an E\\\" \\\"a  P\\\" \\\"an L\\\" \\\"What do you get?\\\" \\\"REPL!\\\"])\"])"
  [fname statements]
  (let [results (into []
                      (comp (map (fn [statement]
                              (eval (read-string statement))))
                            (map #(clojure.string/trim
                                   (with-out-str
                                     (clojure.pprint/pprint %))))
                            (map (fn [s]
                                   (->> s
                                        clojure.string/split-lines
                                        (map #(str ";; " %))
                                        (clojure.string/join "\n")))))
                      statements)
        lines (interleave (map #(str "> " %) statements)
                          results)]
    (make-code-gif fname (clojure.string/join "\n" lines))))


(defn -main [& args]
  (require 'clojure.repl)
  (make-code-gif "example.gif"
                 (clojure.repl/source-fn (or (symbol (first args))
                                             'filter))))

(comment
  (repl-to-gif "repl.gif"
               ["\"Hello World!\""
                "(repeat 7 \"I am using the REPL! 💪\")"
                "(map (fn [s]
         (if (< (count s) 5)
           (str \"Give me \" s \"! ~•~ \" (last s) \"!\")
           s))
       [\"an R\" \"an E\" \"a  P\" \"an L\" \"What do you get?\" \"REPL!\"])"]))

