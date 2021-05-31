(ns me.ttay.thomas-gemdown.parser
  (:require [me.ttay.parser-combinators.combinators :as c]))

;; Thomas' Gem Text Markdown format
;; Parses a subset of markdown into gemtext

;; -------------------- Making Tokens ---------------------------
;; in its own section, so I can change the output format easier later on if needed.
;; Hiccup is industry standard, so should be safe, though.

(defn make-link [name url] [:link name url])
(defn make-image-link [name url] [:image-link name url])
(defn make-text [s] [:text s])
(defn make-footnote-ref [num] [:footnote-ref num])
(defn make-footnote [num text] [:footnote num text])
(defn make-bullet [text] [:bullet text])
(defn make-ordered-list [num text] [:ord num text])
(defn make-blockquote [s] [:blockquote s])
(def blankline [:blankline])
(defn make-line [elts] (if (seq elts) (apply vector :line elts) blankline))
(defn make-lines [lines] (apply vector :lines lines))
(defn make-header [m] [:header m])
(defn make-gemdown [header lines] [:gemdown header lines])

;; Boolean fns
(defn image-link? [tok] (= :image-link (first tok)))
(defn link? [tok] (= :link (first tok)))
(defn line? [tok] (= :line (first tok)))
(defn text? [tok] (= :text (first tok)))
(defn footnote? [tok] (= :footnote (first tok)))
(defn footnote-ref? [tok] (= :footnote-ref (first tok)))

;; -------------------- Hugo Headers ---------------------------
(def after-colon-til-eol
  "Parses everything after a colon, then spaces, then the text til EOL"
  (c/ap-last (c/pchar \:) c/spaces c/til-eol))

(def header-marker (c/skip (c/pstr "---") c/ws))
(def header-key (c/many1-satisfy #(Character/isLetter %)))
(def header-kv (c/ap-vec header-key after-colon-til-eol))
(def header-kv-map (c/fmap-> (c/many1 (c/ap-first header-kv c/ws))
                             #(into {} %)))

(def header (c/fmap-> (c/between header-marker header-kv-map header-marker)
                      make-header))

;; -------------------- Markdown (line) ---------------------------
;; # LL Grammar
;; ## Lexer
;; T_Url  : [a-zA-Z0-9\-?.:/]+
;; T_Decimal  : [0-9]+
;; T_NoSqrString : [^\[\]]*
;; T_NoSqrCloseString : [^\]]*
;; T_NoSpecialStartString : [^!\[]*
;; T_Newline : [(\r\n)|(\n)]
;; 
;; ## Parser
;; ImageLink     : ! Link
;; Footnote      : [^ T_Decimal ]
;; 
;; Link          : [ T_NoSqrString ] ( T_Url )
;; RegularBracketed : [ T_NoSqrCloseString ]
;;                  | [ T_NoSqrCloseString
;;                  
;; Line : ImageLink Line
;;      | Footnote Line
;;      | Link Line
;;      | RegularBracketed Line
;;      | T_NoSpecialStartString Line
;;      | T_Newline

(def sqr-open (c/pchar \[))
(def sqr-close (c/pchar \]))
(def url-chars (c/many1-satisfy #(or (Character/isLetterOrDigit %)
                                     (#{\- \? \. \: \/} %))))

(def link-or-str
  "Upon encountering a square bracket, it can be uncertain whether or not it is just a regular square bracket,
   or if it is a link. We need to lookahead, basically.
   e.g.
   [this is just regular text]
   [this [is just regular text] since nested squares are disallowed]
   [It's ok to mismatch square brackets! I do it all the time
   [this is a link]   (link_url)
   Notice how we can parse the in-square text only conditionally based on whether or not it is followed by a
   round bracket.
   "
  (c/ap-last
   sqr-open
   ;; Bind (>>=) lets us conditionally use the result of the in-square-text to return different types of results
   (c/>>= (c/many-notof "[]\n")
          (fn [in-square-text]
            (c/choice
             ;; If afterwards, we parse ], spaces and then (, we have a link.
             (c/fmap-> (c/try-ap-last sqr-close
                                      c/spaces
                                      (c/between-parens url-chars))
                       #(make-link in-square-text %))
             ;; else, we have parsed a string in square brackets. Add the brackets back.
             (c/ap-last sqr-close
                        (c/preturn (make-text (str "[" in-square-text "]"))))
             ;; if we have a [, we have a square bracket within a square bracket. 
             ;; From hereon til EOL, it can only be a string.
             ;; Parse until end of line.
             (c/fmap-> (c/ap-last sqr-open c/til-eol-not-eating-newline)
                       #(make-text (str "[" in-square-text "[" %)))
             ;; if we see a \n, then reached the EOL. Don't consume it.
             (c/ap-last (c/followedby c/pnewline)
                        (c/preturn (make-text (str "[" in-square-text))))
             ;; Theoretically, we should never reach this point.
             (c/throw-combinator-err-with-stream "Ran out of choices..."))))))

(def image-link
  "Parses an image name, then a round bracket in parentheses, surrounding a URL.
   e.g.
   ![url_name]  (my_url_is_here)"
  (c/ap-last (c/pstr "![")
             (c/fmap
              make-image-link
              (c/ap-first (c/many-notof "]\n") sqr-close c/spaces)
              (c/between-parens url-chars))))

(def footnote-box
  "A footnote box is the box [^12345]. Only decimals allowed."
  (c/between (c/pstr "[^") c/pdecimal sqr-close))

(def footnote-ref
  "A footnote is like this: [^12345]. Only decimals are allowed within."
  (c/fmap-> footnote-box
            make-footnote-ref))

(def regular-text
  "Any text that cannot begin any of the special text parsing formats above."
  (c/fmap-> (c/many1-notof "![\n") make-text))

(def single-nonregular-char
  (c/fmap-> (c/panyof "![") make-text))

;; -------------------- Types of lines ---------------------------
(def in-line-text
  "A single line of gemtext, which can be broken up by links, image links, footnotes."
  (c/many (c/choice
           regular-text
           image-link
           footnote-ref
           link-or-str
           single-nonregular-char)))

(def line-text
  "A single line of gemtext, ended by a newline."
  (c/fmap-> (c/ap-first in-line-text c/pnewline)
            make-line))

(def blockquote
  "The only multi-line construct in gemtext. Three backticks, then parse until three new backticks on a new line."
  (c/fmap-> (c/between (c/pstr "```") (c/str-til "\n```") c/pnewline)
            make-blockquote))

(def footnote-line
  "A footnote box, followed by a colon
   e.g. [^1]: This is my footnote!"
  (c/fmap make-footnote
          footnote-box after-colon-til-eol))

(def bullet-line
  "A bulleted line. A bullet is either - or *"
  (c/fmap->
   (c/ap-last (c/panyof "-*") c/spaces c/til-eol)
   make-bullet))

(def ordered-list-line
  "A numbered line. Starts with an integer, then a dot."
  (c/fmap
   make-ordered-list
   (c/try-ap-first c/pdecimal (c/pchar \.))
   (c/ap-last c/spaces c/til-eol)))

(def gemdown-line
  "A gemdown line is one of the line types mentioned."
  (c/choice blockquote
            footnote-line
            bullet-line
            ordered-list-line
            line-text))

(def gemdown
  "Gemdown is an optional header, followed by many lines."
  (c/fmap make-gemdown
          header
          (c/fmap-> (c/many1 gemdown-line) make-lines)))

;;    ------------------- Scratch ---------------------

(comment
  (c/run (c/ap-last
          (c/many-notof "[]")
          (c/try-ap-last c/spaces (c/between-parens url-chars))) "a]    (asd)")
  (c/run header "---
                 potato: asd
                 aas: a1
                 ---")
  (c/run image-link "![asd] (as)")
  (c/run regular-text "I am regular text here.![asd] [urlhere oops!  aasd")
  (c/run line-text "I am regular text here. ![asd](url) \n")
  (c/run line-text "I am regular text here. asd](url) \n")
  (c/run line-text "I am regular text here. [123] \n") ; TODO this fails!
  (c/run line-text "I am regular text here. [123 (asdsa) \n")
  (c/run line-text "I am regular text here. [^123] \n")
  (c/run line-text "I am regular text here. [^123 \n")
  (c/run line-text "I am regular text here. ![asd](url \n")

  (c/run (c/many1 (c/ap-last (c/pstr "[^") (c/ap-first c/pdecimal (c/pchar \])))) "[^123 \n")

  (c/run blockquote "```
                     Hello, world!
                     This is thomas tay`` `")
  (c/run blockquote
         "```
Hello, world!
This is thomas tay
```
")
  (c/run line-text "I can also have inline links [my_link](https://example.com) and it works fine.\n\nImage links are ok too: ![img] (https://example.com/image.jpg) but they go on a new line.\n\nFootnotes are also fine! [^1]\n\n[^1]: I am the note\n\nIf you [forgot to close a [ bracket, thats fine as long] as it's not a link\n")
  (c/run gemdown (slurp "./resources/test1.md"))
  (time (c/run-parser gemdown (slurp "./resources/test1.md")))
  (c/run (c/many1 gemdown-line) "If you [forgot to close a [ bracket, thats fine as long] as it's not a link\n") ; TODO fails
  ;;
  )
