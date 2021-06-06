(ns me.ttay.thomas-gemdown.emit
  (:require [clojure.string :as str]
            [me.ttay.parser-combinators.combinators :as c]
            [me.ttay.thomas-gemdown.parser :as parser]
            [me.ttay.thomas-gemdown.passes :as passes]))

;; -----------------------------------------------------------------
;; -------------------- Gemtext emission ---------------------------
;; -----------------------------------------------------------------

;; To emit gemtext, I need a vector with each elt being a hiccup tagged vector
;; Each element represents a line. In gemtext, a line can be of type:
;;   :text / :link / :bullet / :blockquote
;; Text is just text. Links are reprsented with =>, bullets with *, blockquotes with ```
;; We first buffer the entire gemtext file as a string, then write it to disk.
;; In the future, I may optimize it to write line by line, to some buffered output stream.

(defn lines->str
  "Given a list of lines, turns it into a string"
  [lines]
  (->> (map
        (fn [line]
          (let [tag (first line)]
            (case tag
              :text (str (line 1))
              :link (str "\n=> " (line 2) " " (line 1)) ; TODO align this somehow?
              :bullet (str "* " (line 1))
              :blockquote (str "```" (line 1))
              :blankline ""
             ; TODO ordered lists? Renumber them? or should that be part of the passes?
              :ord (str (line 1) ". " (line 2)))))
        lines)
       (str/join \newline)))

(defn references->str
  "Given a list of references, turns it into a string."
  [references num-image-links]
  (->> (map (fn [reference i]
              (let [tag (first reference)]
                (case tag
                  :link (str "=> " (reference 2) " " i ". " (reference 1)))))
            references
            (range (inc num-image-links) (+ (count references) num-image-links 1)))
       (str/join \newline)))

(defn footnote-num [footnote] (footnote 1))

(defn footnotes->str
  "Given a list of footnotes, turns it into a sorted list of footnotes."
  [footnotes]
  (->> footnotes
       (sort-by footnote-num)
       (map (fn [footnote]
              (let [tag (first footnote)]
                (case tag
                  :footnote (str (footnote 1) ") " (footnote 2))))))
       (str/join \newline)))


(defn parse-file
  [file-str]
  (let [[_, header, lines] (->> (c/run-parser parser/gemdown file-str)
                                (c/unwrap-success))]
    {:header header, :lines lines}))

(def ^:private extra-vertical-space "\n\n")

(defn lines->gemtext
  [num-image-links, {:keys [lines, references, footnotes]}]
  (let [lines-str (lines->str lines)
        references-str (references->str references num-image-links)
        footnotes-str (footnotes->str footnotes)]
    (str lines-str extra-vertical-space
         "# References\n"
         references-str extra-vertical-space
         "# Footnotes\n"
         footnotes-str)))

(defn file->gemtext [file-str]
  (let [{:keys [lines]} (parse-file file-str)
        num-image-links (passes/count-image-links lines)]
    (->> lines
         (passes/second-pass num-image-links)
         (lines->gemtext num-image-links))))

(comment
  (println (lines->str [[:text "hello, world!"]
                        [:bullet "bullet1"]
                        [:bullet "bullet2"]
                        [:blockquote "js\nmynamekeyes\n```"]
                        [:bullet "bullet3"]
                        [:link "my link here" "https://examples.com"]]))
  (def gemlines2 (->> (c/run-parser parser/gemdown (slurp "resources/test2.md"))
                      (:thomastay.clj-terser.combinators/success)
                      (nnext)
                      (first)))
  (def secondp (passes/second-pass gemlines2 (passes/count-image-links gemlines2)))
  (def mylines (:lines secondp))
  (println (lines->str mylines))
  (println (references->str [[:link "my_link" "https://example.com"]
                             [:link "my_link" "https://zombo.com"]
                             [:link "oopt" "https://example.com"]]))
  (println (footnotes->str [[:footnote 2 "I am the second note"]
                            [:footnote 1 "I am the first note"]
                            [:footnote 3 "I am the third note"]]))

  ;;
  )
