(ns me.ttay.parser-combinators.combinators
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import [me.ttay.parser_combinators CharStream CharStream$CharStreamState]))

;;;;;;;;;;;;;;;;;;;;; COMBINATORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; In this file, we use a very simple version of parser combinators
;; The API design of the Parser combinators is highly inspired by FParsec.
;; A success parser returns a map with this shape
;; {::success Object}
;; 
;; A failure parser returns a map with this shape
;; {::errs ErrorType}
;; It can also contain an optional :message field
;; 
;; A parser takes a stream (anything with a text and position field is considered a stream)
;; and returns a Reply. A reply is a pair of a success/failure maps and the resulting Stream.
;; 
;; Note that all parsers mutate the given stream.
;; It's just a fact of life: parsing is an inherently stateful activity.
;; Any attempt to hide it just results in the state monad and it is no clearer than a simple object that
;; can be easily serialized.


;;;;;;;;;;;; Helpers

(defn- infinite-loop-check [init-tag stream]
  (when (= init-tag (.getStateTag stream))
    (throw (ex-info "Infinite loop detected" {:pos (.getState stream)} ; TODO better errors
                    ))))

(defn changed-parser-state? [^CharStream stream
                             ^CharStream$CharStreamState init-state]
  (not= (.getStateTag stream) init-state))

(defn tag-then-parse
  "Gets the state tag, then applies p to the stream.
   Returns the init-tag and the result parsing `p`"
  [^CharStream stream, p]
  (let [init-tag (.getStateTag stream)]
    [init-tag, (p stream)]))

;; Credit to https://github.com/flatland/useful/blob/develop/src/flatland/useful/utils.clj#L26
(defmacro returning
  "Compute a return value, then execute other forms for side effects.
  Like prog1 in common lisp, or a (do) that returns the first form."
  [value & forms]
  `(let [value# ~value]
     ~@forms
     value#))

(defn default-escape-str "Escapes s using char-escape-string" [s] (str/escape s char-escape-string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Error Handling ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Note: you probably don't want to peruse this first time, goto Parser Primitives to get started
(defn charstreamstate->map
  [^CharStream$CharStreamState state]
  #:charstream{:position (.position state)
               :line-begin (.lineBegin state)
               :line (.line state)})

(defn make-success
  "Makes a success result"
  ([v] {::success v})
  ([v errs] {::success v, ::errs errs}))

(def unwrap-success
  "Given a parse result, extract out the success. Only for external use.
   Internal use should use ::success"
  ::success)

(defn expected-err [msg] {::errs [{::errtype ::expected ::errmsg msg}]})
(defn expected-str [msg] {::errs [{::errtype ::expected-str ::errmsg msg}]})
(defn unexpected-err [msg] {::errs [{::errtype ::unexpected ::errmsg msg}]})
(defn unexpected-str [msg] {::errs [{::errtype ::unexpected-str ::errmsg msg}]})
(defn message-err [msg] {::errs [{::errtype ::message ::errmsg msg}]})
(defn nested-err [^CharStream stream, errs]
  {:pre [(seq errs)]}
  {::errs [{::errtype ::nested-err
            :charstream/state (.getState stream)
            ::errs errs}]})

;; These functions return a vector of errors, to be used inside the ::errs tag
;; Mostly used for testing.
(defn expected-err-inner [msg] [{::errtype ::expected ::errmsg msg}])
(defn expected-str-inner [msg] [{::errtype ::expected-str ::errmsg msg}])
(defn unexpected-err-inner [msg] [{::errtype ::unexpected ::errmsg msg}])
(defn unexpected-str-inner [msg] [{::errtype ::unexpected-str ::errmsg msg}])
(defn message-err-inner [msg] [{::errtype ::message ::errmsg msg}])
(defn nested-err-inner [^CharStream stream, errs]
  {:pre [(seq errs)]}
  [{::errtype ::nested-err
    :charstream/state (.getState stream)
    ::errs errs}])

(def empty-message-err (message-err ""))

(defn- dispatch-err [msg errtype]
  (case errtype ; Note: cannot create nested errors
    ::expected (expected-err msg)
    ::expected-str (expected-str msg)
    ::unexpected (unexpected-err msg)
    ::unexpected-str (unexpected-str msg)
    ::message (message-err msg)))

(defn fatal-err?
  "returns true if v is a fatal error."
  [v]
  {:pre [(and (not (::success v)) (::errs v))]}
  (::fatal v))

(defn merge-errs
  "Merges the errors in the ::errs key of the given result with a seq of errors, or nil"
  [result errs]
  (if (seq errs)
    (update result ::errs concat errs)
    result))

(s/def ::errtype keyword?)
(s/def ::errmsg string?)
;; (s/def :charstream/line-begin int?)
;; (s/def :charstream/line int?)
;; (s/def :charstream/position int?)
(s/def :charstream/state #(instance? CharStream$CharStreamState %))

(defmulti error-type
  "The type of errors. There are:
   1. Expected errors - this is generated when the parser expects something and does not get it
   2. Expected string constant - generated when the parser expects a string
   3. unexpected and unexpected-str are versions of the above
   4. message - when the parser is given a custom error message
   5. nested error - Usually in backtracking scenarios."
  ::errtype)
(defmethod error-type ::expected [_] (s/keys :req [::errmsg]))
(defmethod error-type ::expected-str [_] (s/keys :req [::errmsg]))
(defmethod error-type ::unexpected [_] (s/keys :req [::errmsg]))
(defmethod error-type ::unexpected-str [_] (s/keys :req [::errmsg]))
(defmethod error-type ::message [_] (s/keys :req [::errmsg]))
(defmethod error-type ::nested-err [_] (s/keys :req [:charstream/state ::errs]))

;; The base of all error types
;; Errors are just a seq of errors. Each error in the seq must fulfil this spec.
(s/def ::errs (s/coll-of #(s/valid? ::err %)))
(s/def ::err (s/multi-spec error-type ::errtype))


(defn format-err-pos
  "Given as input a stream, formats where it currently points to
   e.g. Assuming stream is as follows:
   s: 233r
   Returns this string:
   ```
   --> :temp::1:3
   1 |  233r
     |     ^--- Parser stopped here 
   Error:  (Found r, which is not any of 0123456789ABCDEFabcdef)
   ```
   "
  [^CharStream stream]
  (let [init-state (charstreamstate->map (.getState stream))
        filename (.filename stream)
        init-pos (.getStreamPosition stream)
        line-begin (:charstream/line-begin init-state)
        line (:charstream/line init-state)
        col (- init-pos line-begin)
        eol-pos (.nextEOLPos stream init-pos) ; TODO line could be very long! Limit with heuristics!
        s (.getStream stream)
        line-str (subs s line-begin eol-pos)]
    (str " --> " filename ":" line ":" col "\n"
         line " |  " line-str "\n" ; TODO do padding better
         "  |  " (str/join (repeat col " ")) "^--- Parser stopped here")))

(declare err->string)
(defn errs->string
  "Takes as input a seq of errors, and returns it as a string with each
   error on its own line, separated by \\n newlines"
  [errs]
  (->> (map err->string errs)
       (str/join \newline)))

(defn err->string
  "Takes as input an error (i.e. the stuff inside the seq, not a parser result!)
   Defines how errors are printed to the console"
  [err]
  ; TODO this asssumes there's only one error. Should have a case with merging errors...
  (if (not (s/valid? ::err err))
    (do (s/explain ::err err)
        (throw (RuntimeException. "Error does not match spec.")))
    (case (::errtype err)
      ::expected (str "Error: Expected " (::errmsg err))
      ::expected-str (str "Error: Expected string " (::errmsg err))
      ::unexpected (str "Error: Unexpected " (::errmsg err))
      ::unexpected-str (str "Error: Unexpected string " (::errmsg err))
      ::message (str "Error: " (::errmsg err))
      ;; TODO make the backtracking error printing nicer
      ::nested-err (str "Parser backtracked at " (charstreamstate->map (:charstream/state err)) "\n" (errs->string (::errs err))))))

#_((s/valid? :error/errors {:error/type :error/expected :error/message "asd"})
   (s/valid? :error/errors #:error{:type :error/nested-error
                                   :message "asd"
                                   :charstream/state {:charstream/position 0
                                                      :charstream/line-begin 0
                                                      :charstream/line 1}
                                   :errors {:error/type :error/expected :error/message "asd"}}))

(defn throw-combinator-err-with-stream
  "Only for catching bugs and assertion failures"
  [msg]
  (fn [^CharStream stream]
    (throw (ex-info msg {:stream stream
                         :state (charstreamstate->map (.getState stream))}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Parser primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Formally, the following set of primitives defines the Parser monad.
;; However, it's recommended you don't think of Parsers as monads! 
;; `bind` is often not the combinator you want. For that reason, it is implemented (>>=) and not as a word.
;; The same goes for `ap`, implemented as <*>
;; For general use, you should use `preturn`, `ap-first` and `choice`
;; If your use case fits into the specialized parsers like `many`, `sepBy`,
;; you should use those, since they may have optimized implementations.
;; 

(defn pfail
  "The failing parser. Fails with the given error message (and optional type).
   See the spec above for known error typs"
  ([msg] (pfail msg ::message))
  ([msg errtype]
   (let [etype (dispatch-err msg errtype)]
     (fn [_] etype))))

;; Commented out since I haven't gotten the error done properly
;; (defn pfatal
;;   "Fails fatally. Only relevant for certain parsers TBD"
;;   ([msg] (pfatal ::message msg))
;;   ([msg errtype] (fn [_] (merge (dispatch-err msg errtype) {::fatal-err true}))))

(def pzero
  "The failing parser. Always fails"
  (pfail "pzero"))

(declare fmap) ; no circular deps, since fmap depends only on the 1-arg `preturn` (aka pure)

(defn preturn
  "Also known as `return`, or `pure`
   Given any input x, returns a Parser<x>
   If passed two or more parameters, the second and onward parameters
   are taken to be Parsers. Applies those parsers to the stream,
   and returns x.
   In this case, also known as <$
   preturn :: a -> Parser<a>
   preturn :: a -> ...Parser<b> -> Parser<a>
   "
  ([x] (fn [_] (make-success x)))
  ([x p] (fmap (fn [_] x) p))
  ([x p1 p2] (fmap (fn [_] x) p1 p2))
  ([x p1 p2 & ps] (apply fmap (fn [_] x) p1 p2 ps)))

(defn >>=
  "Also known as `bind`
   Given a Parser p :: Parser<a>, and a function f :: <a> -> Parser<b>, 
   it returns a Parser<b>.
   >>= :: Parser<a> -> (a -> Parser<b>) -> Parser<b>
   This probably shouldn't be your first choice of a combinator. It's a primitive.
   Consider using ap-first and ap-last."
  [p f]
  (fn [^CharStream stream]
    (let [[init-tag result] (tag-then-parse stream p)]
      (if-let [v (::success result)]
        (let [result2 ((f v) stream)]
          ;; Err type | changed-parser-state? | Result
          ;;  success | true                  | result
          ;;  success | false                 | result and merge errors
          ;;  error   | _                  | result
          (if (and (::success result2)
                   (not (changed-parser-state? stream init-tag)))
            (merge-errs result2 (::errs result))
            result2))
        result))))

(defn fmap
  "Takes as input a function, and applies it to the parser(s).
   Aka <$>
   fmap :: (a -> b) -> Parser<a> -> Parser<b>
   fmap :: (a -> b -> c) -> Parser<a> -> Parser<b> -> Parser<c>"
  ([f p]
   (>>= p (comp preturn f)))
   ; XXX: optimize this
  ([f p1 p2]
   (>>= p1 (fn [a] (fmap (partial f a) p2))))
  ([f p1 p2 p3]
   (>>= p1 (fn [a] (fmap (partial f a) p2 p3))))
  ([f p1 p2 p3 & rest]
   (>>= p1 (fn [a] (apply fmap (partial f a) p2 p3 rest)))))

(defn fmap->
  "Reverse of fmap, which might be clearer in some cases.
   Similar to |>> in FParsec."
  [p f] (fmap f p))

(defn ap-nth
  "Given a vector of parsers, applies them, and returns the result of the nth one. 0 indexed.
   This function is only meant to implement ap-first and ap-last, but is exposed
   in case it is useful to others.
   If any of the parsers fail, the stream is NOT restored to the original state before
   any parsing occured.

   Note: the reason this takes in a vec and not a seq is that IKVReduce is not implemented
   for ArraySeqs
   "
  [n parserVec]
  {:pre [(vector? parserVec)]
   :post [(if-let [v (::success %)] (nil? v) true)]}
  (fn [^CharStream stream]
    (first (reduce-kv
            (fn [[final-result, prev-err] i p]
              (let [[init-tag, parser-result] (tag-then-parse stream p)
                    r (if (= i n) parser-result final-result)] ; accumulate the success result
                (if (::success parser-result)
                  (if (changed-parser-state? stream init-tag)
                    [r, (::errs parser-result)]
                    [r, (concat (::errs parser-result) prev-err)])
                  (reduced
                   (if (changed-parser-state? stream init-tag)
                     [(merge-errs parser-result prev-err), nil]
                     [parser-result, nil])))))
            [nil, nil] ; result, errors
            parserVec))))

(defn ap-first
  "Given a sequence of parsers, applies them, and returns the result of the first one.
   If any of the parsers fail, the stream is NOT restored to the original state before
   any parsing occured.
   Also known as <*
   <* :: Parser<a> -> ...Parser<b> -> Parser<a>"
  ([p1 p2]
   (ap-nth 0 [p1 p2])) ; XXX write optimized impl of this
  ([p1 p2 p3]
   (ap-nth 0 [p1 p2 p3]))
  ([p1 p2 p3 & ps]
   (ap-nth 0 (apply vector p1 p2 p3 ps))))

(defn ap-last
  "Given a sequence of parsers, applies them, and returns the result of the last one.
   If any of the parsers fail, the stream is NOT restored to the original state before
   any parsing occured.
   Also known as *>
   *> :: ...Parser<a> -> Parser<b> -> Parser<b>"
  ([p1 p2]
   (ap-nth 1 [p1 p2])) ; XXX write optimized impl of this
  ([p1 p2 p3]
   (ap-nth 2 [p1 p2 p3]))
  ([p1 p2 p3 & ps]
   (ap-nth (+ 2 (count ps)) (apply vector p1 p2 p3 ps))))

(defn <*>
  "Takes as input a Parser<a -> b> and Parser<a>,
   and returns a Parser<b>
   Aka `ap`, or the Applicative map
   This probably shouldn't be your first choice of combinator. It's a primitive.
   Consider using ap-first and ap-last."
  ([pf p]
  ; first >>= :: Parser<a->b> -> ((a->b) -> Parser<b>) -> Parser<b>
  ; second >>= :: Parser<a> -> (a -> Parser<b>) -> Parser<b>
   (>>= pf (fn [xf] (>>= p (fn [a] (preturn (xf a)))))))
  ([pf p & ps]
   (reduce <*> pf (apply vector p ps))))

(defn ap-vec
  "Parses any number of parsers into a vector. If any of the parsers fails, returns the latest failure
   without the previous successful parses.
   ap-vec :: Parser<a> -> Parser<b> -> Parser<[a, b]>"
  [p1 p2]
  ; TODO make variadic
  (fn [^CharStream stream]
    (let [[init-tag result] (tag-then-parse stream p1)]
      (if-let [ss1 (::success result)]
        (let [result2 (p2 stream)]
          (if-let [ss2 (::success result2)]
            (if (changed-parser-state? stream init-tag)
              (make-success [ss1, ss2] (::errs result2))
              (make-success [ss1, ss2] (concat (::errs result) (::errs result2))))
            (if (changed-parser-state? stream init-tag)
              result2
              (merge-errs result2 (::errs result)))))
        result))))

(defn skip
  "Ignores the return result of the parsers it parses, unless any fail, in which it returns the error."
  ([p1 p2]
   (fn [^CharStream stream]
     (let [[init-tag result] (tag-then-parse stream p1)]
       (if (::success result)
         (let [result2 (p2 stream)]
           (if (::success result2)
             (if (changed-parser-state? stream init-tag)
               (make-success true (::errs result2))
               (make-success true (concat (::errs result) (::errs result2))))
             (if (changed-parser-state? stream init-tag)
               result2
               (merge-errs result2 (::errs result)))))
         result))))
  ([p1 p2 & ps]
   (fn [^CharStream stream]
     (let [r
           (reduce
            (fn [prev-err p]
              (let [[init-tag result] (tag-then-parse stream p)]
                (if (::success result)
                  (if (changed-parser-state? stream init-tag)
                    (::errs result)
                    (concat prev-err (::errs result)))
                  (if (changed-parser-state? stream init-tag)
                    (reduced result)
                    (reduced (merge-errs result prev-err))))))
            nil ; prev-err
            (apply vector p1 p2 ps))]
       (if (::errs r)
         r ;; if r contains an ::errs field, this means that an error was returned
         (make-success true r) ;; else, this is just prev-err, meaning the skip succeeded.
         )))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Parsing with alternatives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; In this section you will see many references to atomic parsers.
;; An atomic parser is one which does not alter the stream state if the parser fails.
;; All the built in parsers are atomic, but it is possible to construct your own that isn't.
;; In Debug mode (default), the combinators below will assert this atomicity.
;; However, for perf reasons, it is possible to disable the assertions to squeeze maximum performance
;; out of this library. If so, you must ensure you provide atomic parsers.

(defn attempt
  "Tries to parse `p`. If that fails, backtracks and returns the result of `p`.
   Note that you should not use this unless `p` is a non-atomic parser, meaning that
   it modifies the stream state even if it fails."
  [p]
  (fn [^CharStream stream]
    (let [init-state (.getState stream)
          res (p stream)]
      (if (::success res)
        res
        (returning (nested-err stream (::errs res))
                   (.setState stream init-state))))))

(defn choice
  "Attempts to parse p1, but if p1 fails, parses p2, p3, ...
   If p1 fails but changes the parser state, choice will not parse p2 (unless p1 returns a fatal err)"
  ([p1 p2]
   (fn [^CharStream stream]
     (let [[init-tag res] (tag-then-parse stream p1)]
       (if (or (::success res) (changed-parser-state? stream init-tag))
         res
         (let [[second-tag, result2] (tag-then-parse stream p2)]
           (if (changed-parser-state? stream second-tag)
             result2
             (merge-errs result2 (::errs res))))))))
  ([p1 p2 & ps]
   ;; XXX: optimize this into a loop
   (reduce choice (apply vector p1 p2 ps))))

(defn opt
  "Like `choice`, but the first argument is a value and not a parser.
   If the parsers fail, returns Parser<a>.
   Always succeeds.
   opt :: a -> ...Parser<b> -> Parser<a | b>"
  ;; XXX: optimize into a loop
  ([x p]
   (choice p (preturn x)))
  ([x p & ps]
   (choice (apply choice p ps) (preturn x))))

(defn try-ap-nth
  "Like ap-nth, but with one additional difference.
   If the first parser fails, the stream is not restored.
   If the second, third, etc fails AND doesn't change the parser state,
   the stream is restored to its original state.
   "
  [n parserVec]
  {:pre [(vector? parserVec)]
   :post [(if-let [v (::success %)] (nil? v) true)]}
  (fn [^CharStream stream]
    (let [init-state (.getState stream)]
      (first (reduce-kv
              (fn [[final-result, prev-err] i p]
                (let [[init-tag, parser-result] (tag-then-parse stream p)
                      r (if (= i n) parser-result final-result)] ; accumulate the success result
                  (if (::success parser-result)
                    (if (changed-parser-state? stream init-tag)
                      [r, (::errs parser-result)]
                      [r, (concat (::errs parser-result) prev-err)])
                    (reduced
                     (if (changed-parser-state? stream init-tag)
                       [(merge-errs parser-result prev-err), nil]
                       (do
                         (when (not= i 0) (.setState stream init-state)) ; this line is the only diff from ap-nth
                         [parser-result, nil]))))))
              [nil, nil] ; result, errors
              parserVec)))))

(defn try-ap-first
  "Same as ap-first, but backtracks if the second, third, etc. parser 
   fails without changing the parser state. If the first parser fails, reports an error and does not backtrack.
   Note: This parser is intended to improve error reporting since backtracking error reporting is often not useful."
  ([p1 p2]
   (try-ap-nth 0 [p1 p2]))
  ([p1 p2 p3]
   (try-ap-nth 0 [p1 p2 p3])))

(defn try-ap-last
  "Same as ap-first, but backtracks if the second, third, etc. parser 
   fails without changing the parser state. If the first parser fails, reports an error and does not backtrack.
   Note: This parser is intended to improve error reporting since backtracking error reporting is often not useful."
  ([p1 p2]
   (try-ap-nth 1 [p1 p2]))
  ([p1 p2 p3]
   (try-ap-nth 2 [p1 p2 p3])))

(defn followedby
  "Tries to parse p, succeeding if p succeeds and vice-versa.
   Never consumes input.
   followedBy :: Parser<a> -> Parser<a>"
  [p]
  (fn [^CharStream stream]
    (let [init-state (.getState stream)
          [init-tag result] (tag-then-parse stream p)]
      (when (changed-parser-state? stream init-tag)
        (.setState stream init-state))
      result)))

(defn not-followedby
  "Tries to parse p, failing if p succeeds and vice-versa. Does not even attempt to return a nice error message.
   Never consumes input.
   followedBy :: Parser<a> -> Parser<a>"
  [p]
  (fn [^CharStream stream]
    (let [init-state (.getState stream)
          [init-tag result] (tag-then-parse stream p)]
      (when (changed-parser-state? stream init-tag)
        (.setState stream init-state))
      (if (::success result)
        empty-message-err
        (make-success true)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; Parsing Multiples ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- many-impl
  "Returns a parser that parses 0/1 or more of `p`, and depending on require-first?
   might return an empty vec or an error.
   Note: If p changes the parser state and errors out, this parser returns an error too.
   Note: If the parser is a char parser, this will return a vector of chars. If you want
   a string back, use strMany1"
  [p require-first?]
  (fn [^CharStream stream]
    (let [[init-tag result] (tag-then-parse stream p)]
      ; handle first parse separately: needed for many-impl and many-impl-1
      (if-let [v (::success result)]
        (loop [results [v]
               prev-err (::errs result)]
          (let [[init-tag result] (tag-then-parse stream p)]
            (if-let [ss (::success result)]
              (do
                (infinite-loop-check init-tag stream)
                (recur (conj results ss) (::errs result)))
              (if (changed-parser-state? stream init-tag)
                (merge-errs result prev-err)
                (make-success results (concat (::errs result) prev-err))))))
        (if (or require-first? (changed-parser-state? stream init-tag))
          result
          (make-success [] (::errs result)))))))

(defn many1
  "Returns a parser that parses at least one of `p`, and returns a nonempty vector of results.
   If p fails the first time, returns an error.
   Note: p must be atomic. If it is not, either wrap it in `attempt` or use many1!
   Note: If the parser is a char parser, this will return a vector of chars. If you want
   a string back, use many1Satisfy"
  [p]
  (many-impl p true))

(defn many
  "Returns a parser that parses 0 or more of `p`, and returns a vector of results.
   Always succeeds.
   Note: p must be atomic. If it is not, either wrap it in `attempt` or use many1!
   Note: If the parser is a char parser, this will return a vector of chars. If you want
   a string back, use manySatisfy"
  [p]
  (many-impl p false))

(defn many-min-max
  "Parses p a maximum of `max` times. Guarantees that p will not be applied more than `max` times.
   NOTE: once it reaches max, p will not be parsed any more, even if it could be applied further.
   If provided with 3 args, treats it as min and max.
   If p fails changing the parser state, this returns an error.
   If no min is provided, this parser always suceeds."
  ([p max] (many-min-max p 0 max))
  ([p min max]
   {:pre [(<= 0 min max)]}
   (fn [^CharStream stream]
     (let [[result, errs]
           (reduce
            (fn [[results prev-err] i]
              (let [[init-tag result] (tag-then-parse stream p)]
                (if-let [ss (::success result)]
                  [(conj results ss), (::errs result)]
                  (if (or (changed-parser-state? stream init-tag)
                          (< i min))
                    (reduced [nil, result]) ; ok to have empty str since it will probably be replaced.
                    (reduced [results, (concat (::errs result) prev-err)])))))
            [[], nil] ; results, prev-err
            (range max))]
       (if (seq result)
         (make-success result errs)
         errs)))))

(defn- sepby-impl
  "Note: I implemented strs-sepby-impl first. Check that out for implementation notes."
  [p psep require-first?]
  (fn [^CharStream stream]
    ;; parse p
    (let [[init-tag, result1] (tag-then-parse stream p)]
      (if-let [ss1 (::success result1)]
        (loop [final-results [ss1]
               prev-err (::errs result1)]
          ;; --- Parse separator and then p ---
          ;; Note: thanks to the infinite loop check, we may assume that psep OR p consume input if both succeed.
          ;; Thus, we may discard prev-err.
          (let [[first-tag, result2] (tag-then-parse stream psep)
                second-tag (.getStateTag stream)
                psep-changed-stream? (not= first-tag second-tag)]
            (if-let [_ss2 (::success result2)] ; ss2 is kept but not used to have parity with strs-sepby
              ;; parse p
              (let [result3 (p stream)
                    p-changed-stream? (changed-parser-state? stream second-tag)]
                (if-let [ss3 (::success result3)]
                  (do
                    (infinite-loop-check first-tag stream)
                    (recur (conj final-results ss3) ; only conj ss3 since we ignore the result of psep
                           (if p-changed-stream?
                             (::errs result3)
                             (concat (::errs result2) (::errs result3)))))
                  (merge-errs result3
                              (if psep-changed-stream?
                                (::errs result2)
                                (concat prev-err (::errs result2))))))
              (if psep-changed-stream?
                (merge-errs result2 prev-err)
                (make-success final-results (concat prev-err (::errs result2)))))))
        (if (or require-first? (changed-parser-state? stream init-tag))
          (merge-errs (message-err "In strs-sep-by, p did not succeed once.") (::errs result1))
          (make-success [] (::errs result1)))))))

(defn sepby
  "Given two parsers, p and psep, parses the following EBNF grammar:
   (p (psep p)*)?
   Returns a vector of the parsed results from p
   "
  [p psep] (sepby-impl p psep false))

(defn sepby1
  "Given two parsers, p and psep, parses the following EBNF grammar:
   p (psep p)*
   Returns a vector of the parsed results from p"
  [p psep] (sepby-impl p psep true))


(defn between
  "Given pOpen, p, pClose, applies these 3 in sequence and returns p"
  [pOpen p pClose]
  (ap-nth 1 [pOpen p pClose]))

;;;;;;;;;;;; Parsers for chars and strings ;;;;;;;;;;;;;;;;;

(def EOS "End of Stream character" CharStream/endOfStreamChar)
(def unexpected-eos (unexpected-err "end of stream"))

(defn pchar
  "Returns a Parser that parses c.
   The Parser returns {::success c} if parsed correctly.
   else it returns an error.
   pchar :: Parser<Character>"
  [c]
  (fn [^CharStream stream]
    (if (.skipChar stream c)
      (make-success c)
      (if (.isAtEof stream)
        unexpected-eos
        (expected-str (str c))))))

(defn pstr
  "Parses a string. Note that this matches the raw string literal in the stream,
   carriage returns and all.
   pstr :: Parser<String>"
  [s]
  (fn [^CharStream stream]
    (if (.skipStrRaw stream s)
      (make-success s)
      (expected-str s))))

(def ws
  "Parses whitespace. Always succeeds, even if no whitespace is parsed.
  ws :: Parser<Boolean>"
  (fn [^CharStream stream]
    (.skipWhitespace stream)
    (make-success true)))

(def eof
  "If at EOF, succeeds, else fails.
   eof :: Parser<Boolean>"
  (fn [^CharStream stream]
    (if (.isAtEof stream)
      (make-success true)
      (expected-err "end of file"))))

(defn satisfy
  "Parses a single char if the function f returns logical true.
   Takes as input an optional label function, that takes as input
  the parsed character and produces a string to use as the message.
   psatisfy :: (c -> bool) -> Parser<char>"
  ([f]
   (fn [^CharStream stream]
     (let [c (.peek stream)]
       (if (= c EOS)
         unexpected-eos
         (if (f c)
           (do (.skip stream)
               (make-success c))
           (message-err "Failed to satisfy f"))))))
  ([f label-fn]
   (fn [^CharStream stream]
     (let [c (.peek stream)]
       (if (= c EOS)
         unexpected-eos
         (if (f c)
           (do (.skip stream)
               (make-success c))
           (message-err (label-fn c))))))))

(defn- many-satisfy-impl
  "Parses until the supplied function `f` returns false. 
   Depending on require-first?, it either fails if f never succeds, or return empty str.
   Note that newlines will be supplied to `f` as \\n only."
  [f require-first? label]
  (fn [^CharStream stream]
    (let [ch (.peek stream)]
      (if (and (not= EOS ch) (f ch))
        ; first check successful, loop until f fails
        (loop [builder (StringBuilder. (str ch))
               ch (.skipAndPeek stream)]
          (if (and (not= EOS ch) (f ch))
            (recur (.append builder ch)
                   (.skipAndPeek stream))
            (make-success (.toString builder))))
        (if require-first?
          (message-err label)
          (make-success ""))))))

(defn many-satisfy
  "Parses until the supplied function `f` returns false. 
   Returns a possibly empty string. Always succeeds.
   Note that newlines will be supplied to `f` as \\n only."
  [f] (many-satisfy-impl f false nil)) ; many-satisfy can never fail, except for fatal errs

(defn many1-satisfy
  "Parses until the supplied function `f` returns false. 
   Returns a non-empty string, or an error if the first invocation of `f` fails.
   Note that newlines will be supplied to `f` as \\n only."
  ([f] (many-satisfy-impl f true "f did not satisfy many1-satisfy"))
  ([f label] (many-satisfy-impl f true label)))

(def spaces
  "Parses spaces and tabs. Always succeeds, even if no whitespace is parsed.
  ws :: Parser<Boolean>"
  (many-satisfy #{\space \tab}))

(def pdecimal
  "Parses a simple decimal number."
  (fmap-> (many1-satisfy #(Character/isDigit %))
          #(Long/parseLong %)))

(defn ptoString
  "Given a parser returning a coll of chars / strings, applies clojure.string/join to them"
  [p]
  (fmap str/join p))

(defn pstr-all
  "Takes as input two string parsers, and returns a parser that concatenates their result.
   Could be implemented as (ptoString (pvec p1 p2))
   Mainly used for impl, but exposed in case it is useful."
  [p1 p2]
  (ptoString (ap-vec p1 p2)))

(defn- assert-stringable
  "Parsers calling stringBuilder.toString() might have issues dealing with integers,
   since it doesn't append the integer itself but the char.
   This is more for my own sanity. Can be disabled (TODO)"
  [x] (assert (or (string? x) (char? x))
              (str (type x) " is not a string or a char")))

;;; WHEW lad
(defn- strs-sepby-impl
  [p psep require-first?]
  (fn [^CharStream stream]
    ;; parse p
    (let [[init-tag, result1] (tag-then-parse stream p)]
      (if-let [ss1 (::success result1)]
        (do (assert-stringable ss1)
            (loop [sb (StringBuilder. (str ss1))
                   prev-err (::errs result1)]
              ;; --- Parse separator and then p ---
              ;; Note: thanks to the infinite loop check, we may assume that psep OR p consume input if both succeed.
              ;; Thus, we may discard prev-err.
              (let [[first-tag, result2] (tag-then-parse stream psep)
                    second-tag (.getStateTag stream)
                    psep-changed-stream? (not= first-tag second-tag)]
                (if-let [ss2 (::success result2)]
                  (do (assert-stringable ss2)
                      ;; Parse p
                      (let [result3 (p stream)
                            p-changed-stream? (changed-parser-state? stream second-tag)]
                        (if-let [ss3 (::success result3)]
                          (do (assert-stringable ss3)
                              (infinite-loop-check first-tag stream)
                              (recur (doto sb (.append ss2) (.append ss3))
                                     (if p-changed-stream?
                                       (::errs result3)
                                       (concat (::errs result2) (::errs result3)))))
                          (merge-errs result3
                                      (if psep-changed-stream?
                                        (::errs result2)
                                        (concat prev-err (::errs result2)))))))
                  (if psep-changed-stream?
                    (merge-errs result2 prev-err)
                    (make-success (.toString sb) (concat prev-err (::errs result2))))))))
        (if (or require-first? (changed-parser-state? stream init-tag))
          (merge-errs (message-err "In strs-sep-by, p did not succeed once.") (::errs result1))
          (make-success "" (::errs result1)))))))

(defn strs-sepby
  "Given two parsers, p and psep, parses the following EBNF grammar:
   (p (psep p)*)?
   The best explanation is an example. This parser is good for parsing
   comma separated values, e.g.
   ```clojure
   (def pcomma-sep-digits (strs-sepby pdigit (pchar ',')))
   ```
   "
  [p psep] (strs-sepby-impl p psep false))

(defn strs-sepby1
  "Given two parsers, p and psep, parses the following EBNF grammar:
   p (psep p)*"
  [p psep] (strs-sepby-impl p psep true))

(defn contains-char?
  "Returns if string s contains char c"
  [s c] (not= -1 (.indexOf s (int c))))

(defn panyof
  "Takes a string s, and returns a Parser that matches any character in s"
  [s]
  (satisfy #(contains-char? s %) #(str "Found " (char-escape-string %) ", which is not any of " (default-escape-str s))))

(defn manyof
  "Takes a string s, and returns a Parser that matches many of any character in s"
  [s]
  (many-satisfy #(contains-char? s %)))

(defn many1of
  "Takes a string s, and returns a Parser that matches many of any character in s"
  ([s]
   (many1-satisfy #(contains-char? s %) (str "Did not match any of " (default-escape-str s))))
  ([s label]
   (many1-satisfy #(contains-char? s %) label)))

(defn many-notof
  "Takes a string s, and returns a Parser that matches any char except any character in s"
  [s]
  (many-satisfy #(not (contains-char? s %))))

(defn many1-notof
  "Takes a string s, and returns a Parser that matches any character except any in s"
  ([s]
   (many1-satisfy #(not (contains-char? s %)) (str "Found char which is present in " (default-escape-str s))))
  ([s label]
   (many1-satisfy #(not (contains-char? s %)) label)))

(defn str-til
  "Reads many characters until the given string, returning the raw string underneath,
   including the given string."
  [s]
  (fn [^CharStream stream]
    (let [result (.readTilStr stream s)]
      (if (= result "")
        (expected-str s)
        (make-success result)))))

(def panychar
  "Parses any character. Newlines are returned as \\n"
  (fn [^CharStream stream]
    (let [c (.read stream)]
      (if (= c EOS)
        unexpected-eos
        (make-success c)))))

(defn followedby-char "Same as followedby, but for a single char" [c] (followedby (pchar c)))
(defn not-followedby-char "Same as not-followedby, but for a single char" [c] (not-followedby (pchar c)))

(def til-eol-not-eating-newline
  "Parses until the end of the line. Next call to the parser will point at the newline."
  (many1-notof "\r\n"))

(def til-eol
  "Parses until the end of the line. Next call to the parser will point to the
   beginning of the next line. Returns a string."
  (fn [^CharStream stream]
    (make-success (.readTilEOL stream))))

(def pletter
  "Parses a single letter.
   pLetter :: Parser<char>"
  (satisfy #(Character/isLetter %)))

(def plowercase
  "Parses a single lowercase letter. Unicode included."
  (satisfy #(Character/isLowerCase %)))

(def puppercase
  "Parses a single uppercase letter. Unicode included."
  (satisfy #(Character/isUpperCase %)))

(def pJavaIdentifierStart
  "Parses if the char matches java identifier start"
  (satisfy #(Character/isJavaIdentifierStart %)))

(def pJavaIdentifierPart
  "Parses if the char matches java identifier part"
  (satisfy #(Character/isJavaIdentifierPart %)))

(defn pnewline
  "Parses if matches a newline (\\r, \\r\\n, \\n)"
  [^CharStream stream]
  (if (.skipNewline stream)
    (make-success true)
    (expected-str "newline (CRLF or LF)")))

(defn between-char
  "Parses a char `ch`, then applies the parser `p`, then matches `ch`.
   Returns the result of p"
  [ch p]
  (let [pch (pchar ch)] (between pch p pch)))

(defn between-square [p] (between (pchar \[) p (pchar \])))
(defn between-curly [p] (between (pchar \{) p (pchar \})))
(defn between-parens [p] (between (pchar \() p (pchar \))))

;;;;;;;;;;;;;;;;;; Parsing of numbers ;;;;;;;;;;;;;;;;;;;;;;;;

(def pdigit
  "Parses a single digit. Returns the digit as a char (not an int)
   pdigit :: Parser<char>"
  (satisfy #(Character/isDigit %)))

(def pdecimal-digit (panyof "0123456789"))

(def punder "Parses an underscore" (pchar \_))

(defn hex-digit? [c] (contains-char? "0123456789ABCDEFabcdef" c))

(def phex-digit (panyof "0123456789ABCDEFabcdef"))

(def hexchar->int
  "Given a hex character, returns its corresponding integer value"
  {\0 0 \1 1 \2 2 \3 3 \4 4 \5 5 \6 6 \7 7 \8 8 \9 9
   \a 10 \A 10 \b 11 \B 11
   \c 12 \C 12 \d 13 \D 13
   \e 14 \E 14 \f 15 \F 15})

(def hex-4
  "Parses 4 hex digits in a row"
  (fmap->
   (many-min-max phex-digit 4 4)
   (fn [digits-vec]
     (let [digits-str (str/join digits-vec)
           val (Long/parseLong digits-str 0 (count digits-str) 16)]
       (char val)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Running parsers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run
  "Runs the parser p on string s, and pretty prints the result to the console"
  [p s]
  (let [stream (CharStream. s)
        result (p stream)]
    (if-let [v (::success result)]
      (println "Success! Returned:" v)
      (println (format-err-pos stream) \newline (errs->string (::errs result))))))

(defn run-parser
  "Runs the parser p on string s, returning the parse result."
  [p s]
  (p (CharStream. s)))

(defn debug-parser
  "Prints entering and leaving statements for a given parser"
  [p name]
  (fn [^CharStream stream]
    (println "Entering parser " name)
    (returning (p stream)
               (println "Leaving parser " name))))

(comment
  (use 'clojure.repl)
  (pst)
  (run (pchar \a) "a")
  (run (pchar \a) "b")

  (defn str-with-logging [& args]
    (println "I am called with"  args)
    (apply str args))


  (def parseCharA (pchar \a))
  (def parseCharB (pchar \b))
  (def parseComma (pchar \,))
  (parseCharA (CharStream. "b"))
  (run (fmap #(Character/isLetter %) parseCharA) "aa")
  (run (preturn "x" parseCharA) "aa")
  (run (fmap str parseCharA parseCharB) "adb")
  (run (<*> (preturn #(Character/isLetter %)) parseCharA) "aa")
  (run (fmap str parseCharA (fmap (constantly "tt") parseCharB)) "ab")

  (run (preturn "x" parseCharA) "aa")
  (run (ap-first parseCharA parseCharB parseCharB) "abc")
  (run (ap-first parseCharA parseCharB parseCharB) "abb")


  (run (many1 parseCharA) "aaaab")
  (run (many parseCharB) "baaaab")
  ((many1-satisfy #(= \a %)) (CharStream. "aaaaab"))
  ((many1-satisfy #(= \b %)) (CharStream. "aaab"))
  (run (many1-satisfy #(= \b %)) "aaab")

  (run (strs-sepby (many1-satisfy #(= \a %)) (pchar \,)) "aaa,aaa,a")
  (run (strs-sepby (many1-satisfy #(= \a %)) (many-satisfy #(= \, %))) "aaa,aaa,a")

  (run hex-4 "2330")
  (run hex-4 "233r")
  (run (ap-last ws eof) "  \r \r\n\r\n\n \n a")
  (run (attempt parseCharA) "b")

  ; from https://www.quanttec.com/fparsec/users-guide/looking-ahead-and-backtracking.html
  (def bInBrackets (between (pchar \[) (pchar \b) (pchar \])))
  (run (choice (ap-vec (pchar \a) bInBrackets) (pstr "ac")) "a[B]") ;; we want this error
  (run (choice (ap-vec (pchar \a) bInBrackets) (pstr "ac")) "ac") ;; oops!
  (run (choice (attempt (ap-vec (pchar \a) bInBrackets)) (pstr "ac")) "a[B]") ;; try #1
  (run (choice (attempt (ap-vec (pchar \a) bInBrackets)) (attempt (pstr "ac"))) "a[B]") ;; try #2
  (run (choice (try-ap-last (pchar \a) bInBrackets) (pstr "ac")) "a[B]") ;; works!
  (run (choice (try-ap-last (pchar \a) bInBrackets) (pstr "ac")) "ac") ;; works!

  (run (skip (pchar \a) (pchar \x) (pchar \a)) "axx")

  ; end comment
  )


