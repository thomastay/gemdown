(ns me.ttay.thomas-gemdown
  (:require
   [me.ttay.parser-combinators.combinators :as c]
   [me.ttay.thomas-gemdown.parser :as parser]
   [me.ttay.thomas-gemdown.passes :as passes]
   [me.ttay.thomas-gemdown.emit :as emit]))

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
    (println lines-str extra-vertical-space
             "# References\n"
             references-str extra-vertical-space
             "# Footnotes\n"
             footnotes-str)))

(defn run-file [filename]
  (let [{:keys [lines]} (parse-file filename)
        num-image-links (passes/count-image-links lines)]
    (->> lines
         (passes/second-pass num-image-links)
         (lines->gemtext num-image-links))))

(defn -main [args] (run-file args))

(comment
  (run-file "resources/test1.md"))
