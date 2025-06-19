;;   Copyright (c) Nicola Mometto, Rich Hickey & contributors.
;;   The use and distribution terms for this software are covered by the
;;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;;   which can be found in the file epl-v10.html at the root of this distribution.
;;   By using this software in any fashion, you are agreeing to be bound by
;;   the terms of this license.
;;   You must not remove this notice, or any other, from this software.

;; (ns ^{:doc "Protocols and default Reader types implementation"
;;     :author "Bronsa"}
;;     cljs.tools.reader.reader-types
;;     (:refer-clojure :exclude [char read-line])
;;     (:require [cljs.tools.reader.impl.utils :refer [char whitespace? newline?]]
;;               [goog.string])
;;     (:import goog.string.StringBuffer))

(defpackage cljs.tools.reader.impl.reader-types
  (:use :cl :clclojure.pvector :clclojure.cowmap :clclojure.protocols)
  ;;(:shadow :char)
  (:shadowing-import-from :clclojure.base
   :do :def :defn :let :ex-info :instance? :defrecord :true :false :identical? :nil? :when-not
   :hash-map :string? :keyword? :vector? :symbol? :set! :inc :dec :str :throw :aget :aset
   :zero? :when-let :assoc :dissoc :conj :disj :merge :with-meta :meta :subs)  
  (:shadowing-import-from :cljs.tools.reader.impl.utils :char :whitespace? :newline?)
  (:shadow :read-char :peek-char :read-line)
  (:local-nicknames (:base :clclojure.base)
                    (:re   :cl-ppcre)
                    (:u    :cljs.tools.reader.impl.utils )))
(in-package :cljs.tools.reader.impl.reader-types)
(named-readtables:in-readtable clj-re:readtable)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader protocols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;we can rename these at some point to avoid shadowing.
(defprotocol Reader
    (read-char (reader)
               "Returns the next char from the Reader, nil if the end of stream has been reached")
  (peek-char (reader)
             "Returns the next char from the Reader without removing it from the reader stream"))

(defprotocol IPushbackReader
    (unread (reader ch)
            "Pushes back a single character on to the stream"))

(defprotocol IndexingReader
  (get-line-number (reader)
                   "Returns the line number of the next character to be read from the stream")
  (get-column-number (reader)
                   "Returns the column number of the next character to be read from the stream")
  (get-file-name (reader)
                 "Returns the file name the reader is reading from, or nil"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reader deftypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(clojure-deftype StringReader
  (s s-len s-pos)
  Reader
  (read-char (reader)
    (when (> s-len s-pos)
      (let (r (cl:char s s-pos)) ;;could use nth, meh.
        (set! s-pos (inc s-pos))
        r)))
  (peek-char (reader)
    (when (> s-len s-pos)
      (cl:char s s-pos))))

;;this might not matter if it's nodejs specific....
;;doesn't appear anywhere else..
;; (deftype NodeReadableReader (readable buf)
;;   Reader
;;   (read-char (reader)
;;              (if buf
;;                  (let (c (aget buf 0))
;;                    (set! buf nil)
;;                    (char c))
;;                  (let  (c (str (.read readable 1)))
;;                    (when c
;;                      (char c)))))
;;   (peek-char (reader)
;;              (when-not buf
;;                        (set! buf (str (.read readable 1))))
;;              (when buf
;;                (char (aget buf 0)))))

(clojure-deftype PushbackReader
    (rdr buf buf-len buf-pos)
  Reader
  (read-char (reader)
             (let (c (if (< buf-pos buf-len)
                         (aget buf buf-pos)
                         (read-char rdr)))
               (when (< buf-pos buf-len)
                 (set! buf-pos (inc buf-pos)))
               (char c)))
  (peek-char (reader)
             (let (c (if (< buf-pos buf-len)
                         (aget buf buf-pos)
                         (peek-char rdr)))
               (char c)))
  IPushbackReader
  (unread (reader ch)
          (when ch
            (if (zero? buf-pos)
                (throw (ex-info "Pushback buffer is full" nil)))
            (set! buf-pos (dec buf-pos))
            (aset buf buf-pos ch))))

;;we can probably handle defn- as a normal defn;
;;maybe change defn to add an automatic export?
(defn normalize-newline (rdr ch)
  (if (identical? #\return ch)
      (let (c (peek-char rdr))
        (when (or (identical? #\formfeed c)
                  (identical? #\newline c))
          (read-char rdr))
        #\newline)
      ch))

(clojure-deftype IndexingPushbackReader
    (rdr line column
     line-start? prev
     prev-column file-name)
  Reader
  (read-char (reader)
    (when-let (ch (read-char rdr))
      (let (ch (normalize-newline rdr ch))
        (set! prev line-start?)
        (set! line-start? (newline? ch))
        (when line-start?
          (set! prev-column column)
          (set! column 0)
          (set! line (inc line)))
        (set! column (inc column))
        ch)))

  (peek-char (reader)
     (peek-char rdr))

  IPushbackReader
  (unread (reader ch)
     (if line-start?
         (do (set! line (dec line))
                (set! column prev-column))
         (set! column (dec column)))
     (set! line-start? prev)
     (unread rdr ch))

  IndexingReader
  (get-line-number (reader)  line)  ;;don't need int here. (int line)
  (get-column-number (reader)  column) ;;(int column)
  (get-file-name (reader) file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Source Logging support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; "Returns an object of the same type and value as `obj`, with its
;; metadata merged over `m`."
;;this seems kind of weird, I guess they call out source specifically hmm.
(defn merge-meta (obj m)
  (let (orig-meta (meta obj))
    (with-meta obj (merge m (dissoc orig-meta :source)))))

;;defn-
;; "Returns a string containing the contents of the top most source
;; logging frame."
(defn peek-source-log (frames)
  (subs (str (base:get frames :buffer)) (first (base:get frames :offset))))

;;defn-
;;  "Logs `char` to all currently active source logging frames."
;;So the buffer here is a string builder, or object array elsewhere.
;;We don't have that in cl, we just concat strings along the way.
;;we use an adhoc stringbuilder type in clclojure.base now.
;;.append -> conj
(defn log-source-char (frames char)
  (when-let (buffer (base:get frames :buffer))
    (conj buffer char)))

;;defn-
;; "Removes the last logged character from all currently active source
;; logging frames. Called when pushing a character back."
(defn drop-last-logged-char (frames)
  (when-let (buffer (base:get frames :buffer frames))
    (setf (slot-value  buffer 'buff)
          (subs (str buffer) 0 (dec (base:count buffer))))))

(clojure-deftype
 SourceLoggingPushbackReader
 (rdr  line  column
  line-start?  prev
  prev-column file-name frames)
 Reader
 (read-char (reader)
            (when-let (ch (read-char rdr))
              (let (ch (normalize-newline rdr ch))
                (set! prev line-start?)
                 (set! line-start? (newline? ch))
                 (when line-start?
                   (set! prev-column column)
                   (set! column 0)
                   (set! line (inc line)))
                (set! column (inc column))
                (log-source-char (base:deref  frames) ch)
                 ch)))

 (peek-char (reader)
            (peek-char rdr))

 IPushbackReader
 (unread (reader ch)
         (if line-start?
             (do (set! line (dec line))
                 (set! column prev-column))
             (set! column (dec column)))
         (set! line-start? prev)
         (when ch
           (drop-last-logged-char (base:deref frames)))
         (unread rdr ch))

 IndexingReader
 (get-line-number   (reader)  line) ;;int not necessary.
 (get-column-number (reader) column)
 (get-file-name     (reader) file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Public API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;  "Returns true if the reader satisfies IndexingReader"
;; fast check for provided implementations
(defn indexing-reader? (rdr)
  (base:implements? IndexingReader rdr))

;;"Creates a StringReader from a given string"
(defn string-reader
 ((s)
  (StringReader. s (base:count s) 0)))

;;"Creates a PushbackReader from a given string"
(defn string-push-back-reader
  ((s)
   (string-push-back-reader s 1))
  ((s buf-len)
   (PushbackReader. (string-reader s) (base:object-array buf-len) buf-len buf-len)))

;; (defn node-readable-push-back-reader (readable)
;;   (PushbackReader. (NodeReadableReader. readable nil) (object-array 1) 1 1))

;;"Creates an IndexingPushbackReader from a given string or PushbackReader"
(defn indexing-push-back-reader
    ((s-or-rdr)
     (indexing-push-back-reader s-or-rdr 1))
  ((s-or-rdr buf-len)
   (indexing-push-back-reader s-or-rdr buf-len nil))
  ((s-or-rdr buf-len file-name)
   (IndexingPushbackReader.
    (if (string? s-or-rdr)
        (string-push-back-reader s-or-rdr buf-len)
        s-or-rdr)
    1 1 true nil 0 file-name)))

;;"Creates a SourceLoggingPushbackReader from a given string or PushbackReader"
(defn source-logging-push-back-reader
    ((s-or-rdr)
     (source-logging-push-back-reader s-or-rdr 1))
  ((s-or-rdr buf-len)
   (source-logging-push-back-reader s-or-rdr buf-len nil))
  ((s-or-rdr buf-len file-name)
   (SourceLoggingPushbackReader.
    (if (string? s-or-rdr) (string-push-back-reader s-or-rdr buf-len) s-or-rdr)
    1
    1
    true
    nil
    0
    file-name
    (base:atom (hash-map  :buffer (StringBuffer.) :offset '(0))))))

;;"Reads a line from the reader or from *in* if no reader is specified"
;;output is wrong!  hmmm, why isn't stringbuilder accumulating bro?
;;works with defun, not defn!
;;we bail in time.
;; (defun read-line (rdr)
;;   (base:loop
;;     (c (read-char rdr)
;;      s (base::->string-builder ""))
;;     (progn  (print (list  c (str s)))
;;             (if (newline? c)
;;                 (str s)
;;                 (recur (read-char rdr) (conj s c))))))

;;we're not bailing in time.
(defn read-line (rdr)
  (base:loop
    (c (read-char rdr)
     s (base::->string-builder ""))
    (if (newline? c)
        (str s)
        (recur (read-char rdr) (conj s c)))))

(defn source-logging-reader?
    (rdr)
  (instance? SourceLoggingPushbackReader rdr))

;;"Returns true if rdr is an IndexingReader and the current char starts a new line"
(defn line-start?
  (rdr)
  (when (indexing-reader? rdr)
    (= 1 (get-column-number rdr))))

(defn log-source*
    (reader f)
  (with-slots (frames) reader
      (let (buffer (get (base:deref frames) :buffer))
        (base:try
         (base:swap! frames  base:update-in '(:offset) conj (count buffer))
         (let (ret (funcall f))
           (if (implements? IMeta ret)
               (merge-meta ret (hash-map  :source (peek-source-log (base:deref frames))))
               ret))
         ;(catch error e (print "I shouldn't happen, but they forced me to be here in log-source*"))
         (finally
          (base:swap! frames base:update-in '(:offset) base:rest))))))

;;in cljs we have to define macros in clj, not so here.
;;(ns cljs.tools.reader.reader-types)

;; "If reader is a SourceLoggingPushbackReader, execute body in a source
;;   logging context. Otherwise, execute body, returning the result."
(defmacro log-source
  (reader &rest body)
  `(if (and (source-logging-reader? ,reader)
            (not (cljs.tools.reader.impl.utils:whitespace? (peek-char ,reader))))
       (log-source* ,reader (base:fn () ,@body))
       (progn ,@body)))
