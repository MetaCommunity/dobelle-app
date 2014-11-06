;; label.lisp - trivial object label API for applications

(in-package #:application)


;; FIXME: See also UTILS:PRINT-NAME, UTILS:PRINT-LABEL &family

(defgeneric label (object))
;; ^ (values t boolean)
;; See also: UTILS:FORMAT-CONDITION

(defclass labeled-object ()
  ((label
    ;; [sidebar] with this slot definition interpreted as a
    ;; predicate, this slot may resemble an RDF:LABEL property
    :initarg :label)))


(defvar %unnamed%
  ;; primarily for use in format control strings, etc
  ;; FIXME: #I18N
  (coerce "{Unnamed}" 'simple-base-string))


(defmethod label ((object labeled-object))
  ;; (values T BOOLEAN)
  (cond
    ((slot-boundp object 'label)
     (values (slot-value object 'label) t))
    (t (values nil nil))))


(defgeneric princ-label (object stream)
  (:method ((object labeled-object) (stream stream))
    (princ (label object) stream)))
