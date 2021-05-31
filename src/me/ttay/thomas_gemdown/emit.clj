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

(defn lines->gemtext
  [lines]
  (->> (map
        (fn [line]
          (let [tag (first line)]
            (case tag
              :text (str (get line 1))
              :link (str "\n=> " (get line 1) " " (get line 2)) ; TODO align this somehow?
              :bullet (str "* " (get line 1))
              :blockquote (str "```" (get line 1))
              :blankline ""
             ; TODO ordered lists? Renumber them? or should that be part of the passes?
              :ord (str (get line 1) ". " (get line 2)))))
        lines)
       (str/join "\n")))


(comment
  (println (lines->gemtext [[:text "hello, world!"]
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
  (println (lines->gemtext mylines))

  ;;
  )
