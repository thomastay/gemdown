(ns me.ttay.thomas-gemdown.passes-test
  (:require [clojure.test :refer [deftest testing is]]
            [me.ttay.thomas-gemdown.passes :refer
             [second-pass second-pass-line count-image-links]]))

(deftest count-image-links-test
  (testing "Test the counting of image links"
    (is (= 2
           (count-image-links
            [:lines
             [:line [:text "asd"] [:image-link "asd" "asd"] [:text "asd"]]
             [:line [:image-link "my-link" "https"]]])))))

(deftest second-pass-test
  (testing "A single pass through a single line"
    (let [single-line [:line
                       [:text "asd"]
                       [:image-link "asd" "123"]
                       [:text "asd2"]
                       [:link "1" "2"]]
          expected-lines [[:text "asdasd2[3]"]
                          [:link "asd" "123"]]
          expected-references [[:link "1" "2"]]]
      (is (= [expected-lines, expected-references, 4]
             (second-pass-line single-line 3)))))

  (testing "The second pass should transform lines into lines and references. In gemtext, a line is either a link, a line, or a etc."
    (let [lines [:lines
                 [:line
                  [:text "asd"]
                  [:image-link "asd" "123"]
                  [:text "asd2"]
                  [:link "1" "2"]]
                 [:line [:image-link "my-link" "https"]]]
          num-image-links (count-image-links lines)]
      (is (= 2 num-image-links))
      (is (= {:lines [[:text "asdasd2[3]"]
                      [:link "asd" "123"]
                      [:link "my-link" "https"]]
              :references [[:link "1" "2"]]
              :footnotes []}
             (second-pass lines num-image-links)))))

  (testing "Testing second pass with a real gemtext example"
    (let [gemlines [:lines
                    [:line [:text "# Hello"] [:text \!]]
                    [:blankline]
                    [:line [:text "This is just regular markdown"]]
                    [:blankline]
                    [:line [:text "But it can be converted into gemini text"] [:text \!]]
                    [:blankline]
                    [:bullet "thing 1"]
                    [:bullet "thing 2"]
                    [:blankline]
                    [:bullet "thing 3"]
                    [:bullet "thing 4"]
                    [:blankline]
                    [:blockquote "js\nfunction thisIsInABlockQuote() {\n  return \"Hello, world\\n!\";\n}\n```"]
                    [:blankline]
                    [:line [:text "I can also have inline links "] [:link "my_link" "https://example.com"] [:text " and it works fine."]]
                    [:blankline]
                    [:line
                     [:text "Image links are ok too: "]
                     [:image-link "img" "https://example.com/image.jpg"]
                     [:text " but they go on a new line."]]
                    [:blankline]
                    [:line [:text "Footnotes are also fine"] [:text \!] [:text " "] [:footnote-ref 1]]
                    [:blankline]
                    [:footnote 1 "I am the note"]
                    [:blankline]
                    [:line [:text "If you "] [:text "[forgot to close a [ bracket, thats fine as long] as it's not a link"]]]]
      (is (= {:lines
              [[:text "# Hello!"]
               [:blankline]
               [:text "This is just regular markdown"]
               [:blankline]
               [:text "But it can be converted into gemini text!"]
               [:blankline]
               [:bullet "thing 1"]
               [:bullet "thing 2"]
               [:blankline]
               [:bullet "thing 3"]
               [:bullet "thing 4"]
               [:blankline]
               [:blockquote "js\nfunction thisIsInABlockQuote() {\n  return \"Hello, world\\n!\";\n}\n```"]
               [:blankline]
               [:text "I can also have inline links [2] and it works fine."]
               [:blankline]
               [:text "Image links are ok too:  but they go on a new line."]
               [:link "img" "https://example.com/image.jpg"]
               [:blankline]
               [:text "Footnotes are also fine! [note 1]"]
               [:blankline]
               [:blankline]
               [:text "If you [forgot to close a [ bracket, thats fine as long] as it's not a link"]]
              :references [[:link "my_link" "https://example.com"]]
              :footnotes [[:footnote 1 "I am the note"]]}
             (second-pass gemlines (count-image-links gemlines)))))))
