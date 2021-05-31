(ns me.ttay.parser-combinators.combinators-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.spec.alpha :as s]
            [me.ttay.parser-combinators.combinators :refer :all])
  (:import [me.ttay.parser_combinators CharStream]))

;; --------------- Helpers ------------------
(def get-success :thomastay.clj-terser.combinators/success)
(def get-errs :thomastay.clj-terser.combinators/errs)
(def err-spec :thomastay.clj-terser.combinators/err)
(def get-errtype :thomastay.clj-terser.combinators/errtype)
(defn primitive-err? [e] (not= (get-errtype e) :thomastay.clj-terser.combinators/nested-err))
(defn every-truthy? [coll] (every? identity coll))
(defn fulfils-spec-or-print [err]
  (or (s/valid? err-spec err)
      (s/explain err-spec err)))

(declare similar-err-vecs?) ; mutually recursive thanks to nested-err

(defn similar-err?
  "Two primitive errors are considered to be the same if they:
   1. Fulfil the error spec
   2. have the same error type
   3. Are primitive errors OR if not, they have similar nested error vecs.
   The error state and error message are not considered part of the error.
   For nested errors, we perform a recursive check on types, not messages."
  [expected-err parser-err]
  (and
   (fulfils-spec-or-print expected-err) (fulfils-spec-or-print parser-err)
   (= (get-errtype expected-err) (get-errtype parser-err))
   (or (primitive-err? expected-err)
       (similar-err-vecs? (get-errs expected-err) (get-errs parser-err)))))

(defn similar-err-vecs?
  [expected-errs, parser-errs]
  (and (= (count expected-errs) (count parser-errs))
       (every-truthy? (map similar-err? expected-errs parser-errs))))

(defn similar-result?
  "Checks whether two parser results are considered to be the same.
   Two parsers are the same if their success field is ther same,
   and if the errors in the error list are the same.
   Two primitive errors are considered to be the same if they have the same error type.
   The error state and error message are not considered part of the error.
   For nested errors, we perform a recursive check on types, not messages."
  [expected-result parser-result]
  (and (= (get-success expected-result) (get-success parser-result))
       (similar-err-vecs? (get-errs expected-result) (get-errs parser-result))))

;; ====================================================
;; ================ TESTS BEGIN HERE ==================
;; ====================================================
;; Test style guide:
;;   - deftest serves to group tests of a specific area
;;   - testing serves to test a specific angle of that area
;;   - is serves to test a specific functionality of that angle.
;;   - is can have an optional error message, if it isn't clear what the test does.

;; --------------- Char Parser tests ------------------
(deftest pchar-tests
  (testing "pchar parses a char"
    (is (similar-result?
         (make-success \H)
         (run-parser (pchar \H) "Hello, world!")))
    (is (similar-result?
         (expected-str "x")
         (run-parser (pchar \x) "Hello, world!")))
    (is (similar-result?
         unexpected-eos
         (run-parser (pchar \x) "")))))

(deftest pString-tests
  (testing "pString skips ahead by stirngs"
    (is (similar-result?
         (make-success "Hello")
         (run-parser (pstr "Hello") "Hello, world!")))
    (is (similar-result?
         (expected-str "Helloo")
         (run-parser (pstr "Helloo") "Hello, world!"))
        "Parsing too much causes an error")))

(deftest many-tests
  (testing "Test the many parser"
    (is (similar-result?
         (make-success [\a \a \a \a] (expected-str-inner "a"))
         (run-parser (many1 (pchar \a)) "aaaab")))
    (is (similar-result?
         (make-success [\a \a \a \a] (expected-str-inner "a"))
         (run-parser (many (pchar \a)) "aaaab")))
    (is (similar-result?
         (make-success [] (expected-str-inner "b"))
         (run-parser (many (pchar \b)) "aaaab")))
    (is (similar-result?
         (expected-str "b")
         (run-parser (many1 (pchar \b)) "aaaab")))))

(deftest many-satisfy-tests
  (testing "Test the many-satisfy parsers"
    (is (similar-result?
         (make-success "aaaa")
         (run-parser (many-satisfy #(= \a %)) "aaaab")))
    (is (similar-result?
         (make-success "aaaa")
         (run-parser (many1-satisfy #(= \a %)) "aaaab")))
    (is (similar-result?
         (make-success "")
         (run-parser (many-satisfy #(= \b %)) "aaab")))
    (is (similar-result?
         (message-err "")
         (run-parser (many1-satisfy #(= \b %)) "aaab")))))

(deftest strs-sepby-tests
  (testing "strs sep by should parse series of letters sepby a comma, and halt before the end"
    (is (similar-result?
         (make-success "aaa,aaa,a" (get-errs unexpected-eos))
         (run-parser (strs-sepby (many1-satisfy #(= \a %)) (pchar \,)) "aaa,aaa,a")))
    (is (similar-result?
         (message-err "f did not satisfy many1-satisfy")
         (run-parser (strs-sepby (many1-satisfy #(= \a %)) (many-satisfy #(= \, %))) "aaa,aaa,a")))))

(deftest misc-parser-tests
  (testing "hex-4"
    (is (similar-result?
         (make-success \u2330)
         (run-parser hex-4 "2330")))
    (is (similar-result?
         (message-err "Found r, oops")
         (run-parser hex-4 "233r"))))
  (testing "ws parser"
    (is (similar-result?
         (make-success true)
         (run-parser ws "  \r \r\n\r\n\n \n a"))
        "whitespace parser can properly handle CRLF and LF endlines")))

;; --------------- Primitives tests ------------------
(def parseCharA (pchar \a))
(def parseCharB (pchar \b))

(deftest preturn-tests
  (testing "preturn"
    (is (similar-result?
         (make-success "x")
         (run-parser (preturn "x") "aa")))
    (is (similar-result?
         (make-success "x")
         (run-parser (preturn "x" parseCharA) "aa")))))

(deftest applicative-tests
  (testing "Test ap-first, ap-last, ap-nth"
    (is (similar-result?
         (expected-str "b")
         (run-parser (ap-first parseCharA parseCharB parseCharB) "abc")))
    (is (similar-result?
         (make-success \a)
         (run-parser (ap-first parseCharA parseCharB parseCharB) "abb")))))

(deftest fmap-tests
  (testing "Fmap should map a function over a parser"
    (is (similar-result?
         (make-success true)
         (run-parser (fmap #(Character/isLetter %) parseCharA) "aa")))
    (is (similar-result?
         (make-success "ab")
         (run-parser (fmap str parseCharA parseCharB) "ab")))
    (is (similar-result?
         (expected-str "b")
         (run-parser (fmap str parseCharA parseCharB) "adb"))))

  (testing "fmap-> should map a function over a parser's result"
    (is (similar-result?
         (make-success \newline)
         (run-parser (fmap-> (panyof "nrtbfv") #(case % \n \newline)) "n")))))

;; --------------- Applicative tests ------------------

(deftest choice-tests
  (testing "attempt should return success and a nested error"
    (is (similar-result?
         (nested-err (CharStream. "b") (expected-str-inner "a"))
         (run-parser (attempt parseCharA) "b")))))


(comment
  (make-success true (nested-err-inner (CharStream. "b") (expected-str-inner "a")))
  (fulfils-spec-or-print (first (get-err *1))))
