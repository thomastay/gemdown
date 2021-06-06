(ns me.ttay.thomas-gemdown
  (:require
   [me.ttay.parser-combinators.combinators :as c]
   [me.ttay.thomas-gemdown.parser :as parser]
   [me.ttay.thomas-gemdown.passes :as passes]
   [me.ttay.thomas-gemdown.emit :as emit])
  (:gen-class))

(defn parse-file
  [filename]
  (let [[_, header, lines] (->> (c/run-parser parser/gemdown (slurp filename))
                                (c/unwrap-success))]
    {:header header, :lines lines}))

(def extra-vertical-space "\n\n")

(defn lines->gemtext
  [num-image-links, {:keys [lines, references, footnotes]}]
  (let [lines-str (emit/lines->str lines)
        references-str (emit/references->str references num-image-links)
        footnotes-str (emit/footnotes->str footnotes)]
    (str lines-str extra-vertical-space
         "# References\n"
         references-str extra-vertical-space
         "# Footnotes\n"
         footnotes-str)))

(defn file->gemtext [filename]
  (let [{:keys [lines]} (parse-file filename)
        num-image-links (passes/count-image-links lines)]
    (->> lines
         (passes/second-pass num-image-links)
         (lines->gemtext num-image-links))))

(defn run-file [filename]
  (println (file->gemtext filename)))

(defn -main [& args] (run-file (first args)))

(comment
  (spit "resources/test2.gmi" (file->gemtext "resources/test2.md")))
