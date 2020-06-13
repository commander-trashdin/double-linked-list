;;;; dl-list.lisp

(in-package #:dl-list)


;; maybe should make public
;; make-load-value?
(declaim (inline make-%node))
(defstruct %node
  (val nil :type t)
  (left nil :type (or null %node))
  (right nil :type (or null %node)))

(deftype index () `(integer 0 4611686018427387901))

;; FIXME this is a bad way to print
;; should make a reader macro probably
;;(defmethod print-object ((obj %node) stream)
;;  (print-unreadable-object (obj stream :type nil)
;;    (format stream "-~a-" (%node-val obj))))

(declaim (ftype (function () (values %node &optional)) %make-origin))
(defun %make-origin ()
  (let ((%origin (make-%node)))
    (setf (%node-left %origin) %origin
          (%node-right %origin) %origin)))


;; #+sbcl inherit from sequence?
(defclass dl-list ()
  ((origin
    :type %node
    :initform (%make-origin))
   (length
    :type index
    :initform 0)))

#||
(defmethod print-object ((obj dl-list) stream)
  (print-unreadable-object (obj stream :type t)
    (loop :with origin := (slot-value obj 'origin)
          :for this := (%node-left origin) :then (%node-left this)
          :until (eq this origin)
          :do (format stream "~a" this))))
||#

;; Constructor
(declaim (ftype (function (&key (:type symbol)) (values dl-list &optional)) make-dl-list))
(defun make-dl-list (&key type)
  "Creates a fresh instance of doubly-linked-list"
  (declare (ignore type))
  (make-instance 'dl-list))


(defun front (dllist)
  "Returns first element of the list"
  (%node-val (%node-left (slot-value dllist 'origin))))

(defun back (dllist)
  "Returns last element of the list"
  (%node-val (%node-right (slot-value dllist 'origin))))

;; Push
(declaim (ftype (function (%node t) (values &optional)) %insert-left-after))
(defun %insert-left-after (node value)
  "Inserts a new node (from value) between node and its left neighbour"
  (with-slots (left) node
    (with-slots (right) left
      (setf right (make-%node :val value :left left :right node)
            left right)))
  (values))

(declaim (ftype (function (%node t) (values &optional)) %insert-right-after))
(defun %insert-right-after (node value)
  "Inserts a new node (from value) between node and its left neighbour"
  (with-slots (right) node
    (with-slots (left) right
      (setf left (make-%node :val value :left node :right right)
            right left)))
  (values))

(declaim (ftype (function (t dl-list) (values t &optional)) push-front))
(defun push-front (value dllist)
  "Pushes a new value to the front of the list"
  (with-slots (origin length) dllist
    (%insert-left-after origin value)
    (incf length)
    value))

(declaim (ftype (function (t dl-list) (values t &optional)) push-back))
(defun push-back (value dllist)
  "Pushes a new value to the back of the list"
  (with-slots (origin length) dllist
    (%insert-right-after origin value)
    (incf length)
    value))


;; pop
(declaim (ftype (function (dl-list) (values t &optional)) pop-back))
(defun pop-back (dllist)
  "Removes the last value for non-empty list; error otherwise"
  (with-slots (origin length) dllist
    (if (eq origin (%node-left origin))
        (error 'simple-error :format-control "There is nothing left to pop")
        (prog1
            (%node-val (%node-right origin))
          (with-slots (right) (%node-right origin)
            (setf (%node-left right) origin
                  (%node-right origin) right)
            (decf length))))))


(declaim (ftype (function (dl-list) (values t &optional)) pop-front))
(defun pop-front (dllist)
  "Removes the first value for non-empty list; error otherwise"
  (with-slots (origin length) dllist
    (if (eq origin (%node-left origin))
        (error 'simple-error :format-control "There is nothing left to pop")
        (prog1
            (%node-val (%node-left origin))
          (with-slots (left) (%node-left origin)
            (setf (%node-right left) origin
                  (%node-left origin) left)
            (decf length))))))




;; Utility (emptyp? something else)
(declaim (ftype (function (dl-list) (values index &optional)) size))
(defun size (dllist)
  "Returns the length of the list"
  (slot-value dllist 'length))

(declaim (ftype (function (dl-list) (values boolean &optional)) emptyp?))
(defun emptyp? (dllist)
  "t if list is empty, nil otherwise"
  (with-slots (origin) dllist
    (eq origin (%node-left origin))))

;; Iteration macro
(defmacro do-dl-list ((var dllist &optional result) &body body)
  "Iterates over the elements of the list, similar to dolist"
  (let ((node (gensym "NODE"))
        (start (gensym "START"))
        (origin (gensym "ORIGIN")))
    `(block nil
       (with-slots ((,origin origin)) ,dllist
         (let ((,node (%node-left ,origin)))
           (declare (type %node ,node) (type %node ,origin))
           (tagbody
              ,start
              (unless (eq ,node ,origin)
                (let ((,var (%node-val ,node)))
                  (setf ,node (%node-left ,node))
                  (locally ,@body))
                (go ,start)))
           (let ((,var nil))
             ,var
             ,result))))))
