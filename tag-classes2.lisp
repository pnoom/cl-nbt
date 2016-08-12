;;;; tag-classes2.lisp - nbt-tag subclasses and related utils

(in-package #:cl-nbt)

(define-tag-classes)

(defun instantiate-tag (id)
  "Given an id, create and return a tag of the corresponding type."
  (make-instance (id->tagtype id)))

(defmethod print-object ((obj nbt-tag) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (if (name obj) (name obj) "-"))))

(defun show (obj &key (indent 0) (show-name t))
  "DIY tag pretty-printing function. TODO: use actual pretty-printer."
  (let ((*print-pretty* t)
	(*print-length* 3))
    (cond
      ((typep obj 'nbt-tag)
       (when show-name
         (format t "~&~v@t~a" indent (if (name obj) (name obj) "-")))      
       (cond
         ((typep obj 'tag-compound)
	  (loop for x in (payload obj) do
	       (show x :indent (+ indent 2))))
         ((typep obj 'tag-list)
	  (if (not (consp (payload obj)))
	      (show (payload obj) :indent (+ indent 2))
	      (loop for x in (payload obj) do
		   (show x :indent (+ indent 2) :show-name nil))))
         (t
	  (show (payload obj) :indent (+ indent 2)))))
      (t
       (format t " ~a" obj)))))

(defun find-tag (target tags)
  "Aux fn for get-tag."
  (if (numberp target)
      (elt tags target)
      (find target tags :key #'name :test #'string=)))

;; Eg:   (get-tag chunk-root-tag "Level" "Sections" 5 "Blocks")

(defun get-tag (root &rest targets)
  "Finds a nested tag by following the targets (indexes for tags directly
   within a tag-list's payload; for all other tags, names) and returns it.
   If only part of the path could be followed, returns the last tag found.
   Does not check if indexes are out of bounds, so be careful with tag-lists.
   Lacks other error-checks."
  (labels ((rec (targets tags &optional (last-tag-found nil))
             (if (or (null targets) (null tags))
                 last-tag-found
                 (let ((x (find-tag (car targets) tags)))
                   (if (not (member (type-of x) '(tag-list tag-compound)))
                       x
                       (rec (cdr targets) (payload x) x))))))
    (rec targets (payload root))))
