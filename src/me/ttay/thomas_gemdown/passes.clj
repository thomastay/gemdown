(ns me.ttay.thomas-gemdown.passes
  (:require [me.ttay.parser-combinators.combinators :as c]
            [me.ttay.thomas-gemdown.parser :as parser]))

;; -----------------------------------------------------------------
;; ------------------ Transforming tree nodes ----------------------
;; -----------------------------------------------------------------

;; A good transformation pass should a few things:
;;
;; # Tier 1 (done!)
;; 
;;   * Move image links into the next line (splice it out of the current line)
;;   * Replace footnote references with [:text "[note %]"].
;;   * Move all footnotes to the Footnotes section.
;;   * Replace inline links with the corresponding number. 
;;     Numbering is from top to bottom, including image links. 
;;     All image links come first, since inline links appear in the footnotes.
;;
;; # Tier 2:
;;   * Verify that all footnote references have a corresponding footnote
;;   * Verify all URLs are valid URIs (using Apache commons)
;;   * Renumber all ordered lists.

;; https://clojuredocs.org/clojure.core/partition-by
;; Credit to Rauhs
(defn split-by
  "Partitions the collection into exactly two [[all-truthy] [all-falsy]]
   collection."
  [p coll]
  (mapv persistent!
        (reduce
         (fn [[t f] x] ; truthy and falsey
           (if (p x)
             [(conj! t x) f]
             [t (conj! f x)]))
         [(transient []) (transient [])] ;  truthy, falsey
         coll)))

;; --------------- First pass ------------------

(def ignore-tag next) ;; alias for readability

(defn count-image-links-in-line
  "In:  [:line [:text asd] [:image-link asd 123] [:text asd] [:image-link 1234]]
   Out: 2"
  [gemline]
  (if (parser/line? gemline)
    (->> gemline
         (ignore-tag)
         (filter parser/image-link?)
         (count))
    0))

(defn count-image-links
  "Returns the number of image links."
  [gemlines]
  (->> gemlines
       (ignore-tag)
       (map count-image-links-in-line)
       (reduce +)))

;; ------------------------- Second pass --------------------------------
;; In the 2nd pass, we use the number of image links to determine the starting value
;; for all regular links. This is because we will place all the regular links
;; at the end, after the image links, so in Amphora the image links number will come first.
;; (see the second-pass test in tgmid_test.clj)

(defn imagelink->link
  "[:image-link a b] ==> [:link a b]"
  [image-link]
  (parser/make-link (image-link 1) (image-link 2)))

;;; For line in lines, 
;;;   for item in line,
;;;     if item is :text, then conj it onto the new line
;;;     if item is :image-link, then store it in a line buffer
;;;     if item is :link, then store it in a global buffer, and conj on
;;;       a :text line with the str as the num-image-links + 1. Update num-image links.
;;;   after processing the line, if there are items in the line buffer, then add it to lines.
;;; After processing the lines, if there are items in the global buffer, place them in the references section.

(defn process-toks->text
  "Processes Tokens. Currently, does two things:
   1) If a token is a link, replaces it with square brackets and the given number.
   e.g. with link-num as 3, and toks: [[:text 3] [:link 1 2]] ==> [[:text 3] [:text '[3]']], link-num: 4
   2) If the token is a footnote ref, replaces it with text. (TODO: add it to some sort of verification list.)
   else, just move along.
   Finally, it joins up all the tokens into a single text token."
  [toks, link-num]
  (-> (reduce (fn [[sb, link-num], item]
                (cond
                  (parser/link? item)         [(.append sb (str "[" link-num "]")), (inc link-num)]
                  (parser/footnote-ref? item) [(.append sb (str "[note " (second item) "]")), link-num]
                  (parser/text? item)         [(.append sb (second item)), link-num]))
              [(StringBuilder.), link-num] ; output-links, link-num
              toks)
      (update 0 (comp parser/make-text #(.toString %)))))

(defn second-pass-line
  "For each line, first skip the tag, then partition out the image links.
   Turn the image links into regular links,
   then for the rest of the tokens, perform a replacement of certain tokens (see process-toks documentation)
   Returns a vec of the new lines, any links to put in the reference section, and the new number of links."
  [line link-num]
  (let [[image-links, rest-toks] (->> line (ignore-tag) (split-by parser/image-link?))
        standalone-links (map imagelink->link image-links)]
    ;; it's possible that a line consists of only image-links, in which case we return the image links as standalone lines directly.
    (if (seq rest-toks)
      (let [[text-line, link-num]     (process-toks->text rest-toks link-num)
            references                (filter parser/link? rest-toks)
            all-lines                 (into [] (cons text-line standalone-links))]
        [all-lines, references, link-num])
      [standalone-links, [], link-num])))

(defn second-pass-over-lines
  "For line in lines, dispatch on the tag.
   If it is a line, run second-pass-line over it, getting back a vector of lines.
   We then join up those vector of lines into a giant lines vector.
   If it is a footnote, put it in the footnote category"
  [lines starting-num]
  (reduce (fn [{curr-lines :lines, curr-references :references, :keys [starting-num] :as acc}
               line]
            (cond
              (parser/line? line)
              (let [[lines, references, starting-num] (second-pass-line line starting-num)]
                (assoc acc
                       :lines (vec (concat curr-lines lines))
                       :references (concat curr-references references)
                       :starting-num starting-num))
              (parser/footnote? line) (update acc :footnotes conj line)
              :else                   (update acc :lines conj line)))
          {:lines [], :references [], :footnotes [], :starting-num starting-num}
          lines))

(defn second-pass
  "Runs the second pass to move image links onto their own line, collect regular links into a references section,
   and turn regular links into bracketed text."
  [num-image-links, lines]
  (let [{:keys [lines, references, footnotes]} (second-pass-over-lines (ignore-tag lines) (inc num-image-links))]
    {:lines (vec lines), :references (vec references), :footnotes (vec footnotes)}))

(comment
  (def gemlines (->> (c/run-parser parser/gemdown (slurp "resources/test1.md"))
                     (:thomastay.clj-terser.combinators/success)
                     (nnext)
                     (first)))
  (second-pass gemlines (count-image-links gemlines))
  (next [1 2 3])
  (count-image-links
   [:lines
    [:line [:text "asd"] [:image-link "asd" "asd"] [:text "asd"]]
    [:line [:image-link "my-link" "https"]]])
  (count-image-links gemlines)
  (def goal [[:line [:text "asd"] [:text "asd"]]
             [:line [:link "my-link" "https"]]])
  (->> [:line [:text "asd"] [:link "my link" "https://"] [:text "asd"]]
       (next)
       (split-by #(= (first %) :text))
       (map #(into [] (cons :line %))))
  (defn split-line [line]
    (->> line
         (next)
         (split-by #(= (first %) :text))
         (filter seq) ;; hmm. This removes blank lines...
         (map #(into [] (cons :line %)))))
  (->> (c/run-parser (c/many1 parser/gemdown-line) "I can also have inline links [my_link](https://example.com) and it works fine.\n
Here's another line!\n")
       (:thomastay.clj-terser.combinators/success)
       (mapcat split-line))
  (process-toks->text [[:text "asd"]
                       [:text "asd2"]
                       [:link "1" "2"]]
                      2))

