(defpackage :ramarren-utils (:use :common-lisp)
  (:export heapify heap-empty nheap-insert heap-peek nheap-extract))
(in-package :ramarren-utils)

;heap anyway, as an array
(defun heap-parent (i) (truncate (/ (1- i) 2)))
(defun heap-rchild (i) (+ (* 2 i) 2))
(defun heap-lchild (i) (+ (* 2 i) 1))

(defun nheap-insert (what where);destructive
  (declare (inline heap-parent))
  (vector-push-extend what (car where))
  (flet ((wheref (i) (aref (car where) i))
	 ((setf wheref) (nval i) (setf (aref (car where) i) nval)))
  ;percolate
  (do ((el (1- (length (car where))) (heap-parent el)))
      ((or
	(= el 0)
	(funcall (cadr where)  (wheref (heap-parent el)) (wheref el))) where)
      (let ((tel (wheref el)))
	(setf (wheref el) (wheref (heap-parent el)))
	(setf (wheref (heap-parent el)) tel)))))

(defun heap-peek (where) 
  (unless (heap-empty where) 
    (aref (car where) 0)))

(defun heap-empty (where)
  (if (zerop (length (car where)))
    t
    nil))

(defun nheap-extract (where)
  (declare (inline heap-parent heap-rchild heap-lchild))
  (if (heap-empty where) 
    nil
    (flet ((wheref (i) (aref (car where) i))
  	  ((setf wheref) (nval i) (setf (aref (car where) i) nval)))
     (let ((topel (wheref 0))(lght (length (car where))))
      (setf (wheref 0) (wheref (1- lght)))
      (vector-pop (car where))
      ;reheap
      (do ((el 0))
         ((and 
	      (if (< (heap-lchild el) lght)
 	       (funcall (cadr where) (wheref el) (wheref (heap-lchild el)))
	       t)
	      (if (< (heap-rchild el) lght)
	       (funcall (cadr where) (wheref el) (wheref (heap-rchild el)))
	       t))
	  topel)
 	 (let ((tel (wheref el)))
          (if;either both children exist
	    (< (heap-rchild el) lght)
	     (if (funcall (cadr where) (wheref (heap-lchild el))(wheref (heap-rchild el)))
	       (setf (wheref el) (wheref (heap-lchild el))
	 	     (wheref (heap-lchild el)) tel
	 	     el (heap-lchild el))
	       (setf (wheref el) (wheref (heap-rchild el))
	 	     (wheref (heap-rchild el)) tel
		     el (heap-rchild el)))
	     (setf (wheref el) (wheref (heap-lchild el));or just the left one, tree is balanced
		   (wheref (heap-lchild el)) tel
		   el (heap-lchild el)))))))))

(defun heapify (what compar) "Turns list into a heap"
  (let ((arry (list (make-array (max (length what) 20) :adjustable t :fill-pointer 0) compar))
       (mwhat (if (vectorp what) (map 'list #'identity what) what)))
    (dolist (el mwhat) (nheap-insert el arry))
    arry))
