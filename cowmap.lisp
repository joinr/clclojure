;;A lame copy-on-write implementation of persistent maps
;;useful for bootstrapping.

;;Notably, none of the operations on these guys are
;;lazy.  Uses copies for otherwise destructive operations.
;;Wraps a mutable hashtable.
(defpackage :clclojure.cowmap
  (:use :common-lisp)
  (:export :persistent-map
	   :empty-map?
	   :map-count
           :map-assoc
           :empty-map
           :cowmap-table)
  (:shadow :assoc
	   :find))
(in-package clclojure.cowmap)

(EVAL-WHEN (:compile-toplevel :load-toplevel :execute)
  (defstruct cowmap (table (make-hash-table)))
  
;;From stack overflow.  It looks like the compiler needs a hint if we're 
;;defining struct/class literals and using them as constants.
  (defmethod make-load-form ((m cowmap) &optional env)
    (declare (ignore env))
    (make-load-form-saving-slots m)))

(defun ->cowmap ()
  "Simple persistent vector builder.  Used to derive from other pvectors 
   to share structure where possible."
  (make-cowmap))

(common-utils::defconstant! +empty-cowmap+ (make-cowmap))
(defun empty-map  () +empty-cowmap+)
(defun empty-map? (m) (eq m +empty-cowmap+))

(defun map-count (m)
  (hash-table-count (cowmap-table m)))


(defun insert-keys! (tbl xs)
   (assert (evenp (length xs)))
  (loop for (k v) in (common-utils::partition! 2 xs)
        do   (setf (gethash k tbl) v))
  tbl)

(defun persistent-map (&rest xs)
  "Funcallable constructor for building vectors from arglists.  Used for
   read-macro dispatch as well."
  (if (null xs)
      +empty-cowmap+
      (progn
        (assert (evenp (length xs)))
        (let* ((cm (->cowmap))
               (tbl (cowmap-table cm)))
          (insert-keys! tbl xs)
          cm))))

(defun map-contains? (m k)
  (second (values (gethash k (cowmap-table m)))))

(defun map-get (m k &optional default)
  (gethash  k (cowmap-table m) default))

(defun map-entry-at (m k)
  (multiple-value-bind (v present)  (map-get m k)
    (when present (list k v))))

(defun map-assoc (m k v)
  (let ((tbl (common-utils::copy-hash-table (cowmap-table m))))
    (setf (gethash k tbl) v)
    (make-cowmap :table tbl)))

(defun map-dissoc (m k)
  (if (map-contains? m k)      
      (let ((tbl (common-utils::copy-hash-table (cowmap-table m))))
        (remhash k tbl)
        (make-cowmap :table tbl))
      m))

(defun map-seq (m)
  (common-utils::hash-table->entries (cowmap-table m)))

(defmethod print-object ((obj cowmap) stream)
  (common-utils::print-map (cowmap-table obj) stream))
