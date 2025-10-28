(defpackage clclojure.string
  (:use :cl :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  (:shadow :reverse :replace)
  (:shadowing-import-from :clclojure.base :apply
   :do :def :defn :let :ex-info :instance? :defrecord :true :false :identical? :nil? :when-not
   :hash-map :string? :keyword? :vector? :symbol? :set! :inc :dec :str :throw :aget :aset
   :zero? :when-let :assoc :dissoc :conj :disj :merge :with-meta :meta :subs :cond :apply :seq
   :loop :char :char? :next :first :-> :vector :vec :loop)  
  (:shadow :read-char :peek-char :read-line)
  (:local-nicknames (:base :clclojure.base)
                    (:re   :cl-ppcre))
  (:export :reverse :replace :re-quote-replacement :replace-first :join :capitalize
           :upper-case :lower-case :split :split-lines :trim :triml :trimr :trim-newline
           :blank? :last-index-of :index-of :escape :starts-with? :ends-with? :includes?
           ))
(in-package :clclojure.string)
(named-readtables:in-readtable clj-re:readtable)

;   Copyright (c) Rich Hickey. All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;; common lisp port by joinr

;; Design notes for clojure.string:

;; 1. Strings are objects (as opposed to sequences). As such, the
;;    string being manipulated is the first argument to a function;
;;    passing nil will result in a NullPointerException unless
;;    documented otherwise. If you want sequence-y behavior instead,
;;    use a sequence.

;; 2. Functions are generally not lazy, and call straight to host
;;    methods where those are available and efficient.

;; 3. Functions take advantage of String implementation details to
;;    write high-performing loop/recurs instead of using higher-order
;;    functions. (This is not idiomatic in general-purpose application
;;    code.)

;; 4. When a function is documented to accept a string argument, it
;;    will take any implementation of the correct *interface* on the
;;    host platform. In Java, this is CharSequence, which is more
;;    general than String. In ordinary usage you will almost always
;;    pass concrete strings. If you are doing something unusual,
;;    e.g. passing a mutable implementation of CharSequence, then
;;    thread-safety is your responsibility."
;;       :author "Stuart Sierra, Stuart Halloway, David Liebke"

;;^String
;; "Returns s with its characters reversed."
;;string reverse is just cl:reverse.
#-sbcl
(defn reverse (s)
  (cl:reverse s))
;;^String
;; "Given a replacement string that you wish to be a literal
;;    replacement for a pattern match in replace or replace-first, do the
;;    necessary escaping of special characters in the replacement."

(setf (fdefinition 're-quote-replacement) #'clj-re:re-quote-replacement)
;; "Replaces all instance of match with replacement in s.

;;    match/replacement can be:

;;    string / string
;;    char / char
;;    pattern / (string or function of match).

;;    See also replace-first.

;;    The replacement is literal (i.e. none of its characters are treated
;;    specially) for all cases above except pattern / string.

;;    For pattern / string, $1, $2, etc. in the replacement string are
;;    substituted with the string that matched the corresponding
;;    parenthesized group in the pattern.  If you wish your replacement
;;    string r to be used literally, use (re-quote-replacement r) as the
;;    replacement argument.  See also documentation for
;;    java.util.regex.Matcher's appendReplacement method.

;;    Example:
;;    (clojure.string/replace \"Almost Pig Latin\" #\"\\b(\\w)(\\w+)\\b\" \"$2$1ay\")
;;    -> \"lmostAay igPay atinLay\""
(setf (fdefinition 'replace) #'clj-re:re-replace)

;; "Replaces the first instance of match with replacement in s.

;;    match/replacement can be:

;;    char / char
;;    string / string
;;    pattern / (string or function of match).

;;    See also replace.

;;    The replacement is literal (i.e. none of its characters are treated
;;    specially) for all cases above except pattern / string.

;;    For pattern / string, $1, $2, etc. in the replacement string are
;;    substituted with the string that matched the corresponding
;;    parenthesized group in the pattern.  If you wish your replacement
;;    string r to be used literally, use (re-quote-replacement r) as the
;;    replacement argument.  See also documentation for
;;    java.util.regex.Matcher's appendReplacement method.

;;    Example:
;;    (clojure.string/replace-first \"swap first two words\"
;;                                  #\"(\\w+)(\\s+)(\\w+)\" \"$3$2$1\")
;;    -> \"first swap two words\""
(setf (fdefinition 'replace-first) #'clj-re:re-replace-first)

;; "Returns a string of all elements in coll, as returned by (seq coll),
;;    separated by an optional separator."

;;^String
(defn join
  ((coll)   (apply str coll))
  ((separator coll)
   (loop (sb   (base:->string-builder (str (first coll)))
          more (next coll)
          sep  (str separator))
       (if more
         (recur (-> sb (conj sep) (conj (str (first more))))
                (next more)
                sep)
         (str sb)))))

;; ^String
;; "Converts first character of the string to upper-case, all other
;;   characters to lower-case."
(setf (fdefinition 'capitalize) #'string-capitalize)
;;"Converts string to all upper-case."
(setf (fdefinition 'upper-case) #'string-upcase)
;;"Converts string to all lower-case."
(setf (fdefinition 'lower-case) #'string-downcase)

;; "Splits string on a regular expression.  Optional argument limit is
;;   the maximum number of parts. Not lazy. Returns vector of the parts.
;;   Trailing empty strings are not returned - pass limit of -1 to return all."
(defn split
  ((s re) 
   (vec (clj-re:re-split s re)))
  ((s re limit)
   (vec (clj-re:re-split s re limit))))

;; "Splits s on \\n or \\r\\n. Trailing empty lines are not returned."
(defn split-lines
  (s)
  (split s #"\r?\n"))

;;"Removes whitespace from both ends of string."
;;^String

(def whitespace-chars
    '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout))
;;might need to eliminate linefeed, dunno.
;;could be platform specific...
(def newline-chars '( #\Newline #\Linefeed #\Return))

(defn trim
  (s)
  (string-trim whitespace-chars
               s))

;;"Removes whitespace from the left side of string."
(defn triml
  (s)
  (string-left-trim whitespace-chars s))

;;"Removes whitespace from the right side of string."
(defn trimr
  (s)
  (string-right-trim whitespace-chars s))

;; "Removes all trailing newline \\n or return \\r characters from
;;   string.  Similar to Perl's chomp."
(defn trim-newline
    (s)
  (string-right-trim newline-chars s))

;;"Checks whether a given character is whitespace"
(defn whitespace?
    (ch)
  (re::whitespacep ch))

;;"Checks whether a given character is numeric"
(defn numeric? (ch)
  (digit-char-p ch))

;;"Checks whether the character is a newline"
(defn newline? (c)
  (identical? #\newline c))

;;"True if s is nil, empty, or contains only whitespace."
(defn blank? (s)
  (let (n (length s))
    (or (= n 0)
        (loop (idx (dec  n))
              (if (zero? idx)
                  t
                  (when (whitespace? (elt s idx))
                    (recur (dec idx))))))))

;; "Return a new string, using cmap to escape each character ch
;;    from s as follows:
   
;;    If (cmap ch) is nil, append ch to the new string.
;;    If (cmap ch) is non-nil, append (str (cmap ch)) instead."
(defn escape  (s cmap)
  (let (n (length s))
    (loop (index  0
                  buffer (base:->string-builder))
          (if (= n index)
              (str buffer)
              (let (ch (elt s index))
                (if-let (replacement (get cmap ch))
                  (conj buffer replacement)
                  (conj buffer ch))
                (recur (inc index) buffer))))))

(defn as-char (x)
  (cond  (char? x) x
         (and  (stringp x)
               (> (length x) 0))
         (elt x 0)
         :else (throw (ex-info "invalid char-or-single-char-string!" (hash-map :in x)))))

(defn as-charseq (x)
  (cond  (stringp x) x
         (char? x) (str x)
         :else (throw (ex-info "invalid char-or-single-char-string!" (hash-map :in x)))))

;; "Return index of value (string or char) in s, optionally searching
;;   forward from from-index. Return nil if value not found."

(defn index-of
  ((s  value)
   (search (as-charseq value) s))
  ((s value from-index)
   (search (as-charseq value) s :start1 from-index)))

;; "Return last index of value (string or char) in s, optionally
;;   searching backward from from-index. Return nil if value not found."
(defn last-index-of
  ((s  value)
     (search (as-charseq value) s :from-end t))
  ((s value from-index)
   (search (as-charseq value) s :start1 from-index :from-end t)))

;;"True if s ends with substr."
(defn ends-with? (x suffix)
  (and (<= (count suffix) (count x))
       (string= x suffix :start1  (- (length x) (length suffix)))))

;;"True if s starts with substr."
(defn starts-with? (x prefix)
  (and (<= (count prefix) (count x))
       (string= x prefix :end1 (length prefix))))

;;"True if s includes substr."
(defn includes?  (s  substr)
  (when  (search substr s)
    t))
