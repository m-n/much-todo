;;;; much-todo.lisp
;;; This file operates on a single todo file, todo.obj, located within
;;; the project's directory. The file is a textual representation of
;;; our todo items. We implement convenient ways to view the file,
;;; and add and remove items from the file, from the repl.

;;;; preamble

(in-package #:much-todo)

(defmacro with-todo (var &body body)
  `(with-standard-io-syntax
     (let ((*print-readably* t))
       (with-file-datastructure (,var *todo-pathname* :readtable *todo-readtable*)
	 ,@body))))

;;;; Interface

(defun todo (&optional new-todo task)
  "Return the todo from *todo-pathname*, add the new-todo if present to the file."
  (with-todo todo
    (cond ((and new-todo task)
	   (multiple-value-bind (at prev subtaskp) (locate task todo)
	     (declare (ignorable prev subtaskp))
	     (setf (subtask at) (apply 'make-instance 'todo
				       :task new-todo
				       (when (subtask at)
					 (list
					  :next (subtask at)))))))
	  (new-todo
	   (setf todo (make-instance 'todo :task new-todo :next todo)))
	  (t
	   nil))
    todo))


(defun focus (&optional string)
  "Move todo matching string to front of todo file, *todoing* <- next subtask."
  (unless string (setq string ""))
  (with-todo todo
    (when-let ((ordered-todo (nmove-to-front string todo)))
      (setq todo ordered-todo)
      (setq *todoing* (next-task todo)))))

(defun unfocus ()
  "Clear *todoing*"
  (setq *todoing* nil))

(defun finish ()
  "Clear *todoing*, remove the most recent task.
Fixme: doesn't insure that the task named in *todoing* is still the most 
recent task."
  (pop-todo)
  (setq *todoing* nil))

;;;; Support

(defun pop-todo ()
  "Remove the first todo from the list."
  (with-todo todo
    (let ((top todo)
	  (next (next-todo todo)))
      (if (subtask todo)
	  (progn
	    (setf (subtask todo) (next-todo next))
	    (format t "~&Finished:~&~A" (task next)))
	  (progn 
	    (setf todo next
		  (next top) nil)
	    (format t "~&Finished:~&~A" top))))))

(defun nmove-to-front (string todo)
  "If string is a prefix for the task of a todo, move it to the front of todo."
  (when (task-prefix-p string todo)
    (return-from nmove-to-front todo))
  (do ((looking (next todo) (next looking))
       (prev todo looking))
      ((and looking
	    (task-prefix-p string looking))
       (prog1 looking
	 (setf (next prev) (next looking)
	       (next looking) todo)))
    (unless looking (return-from nmove-to-front nil))))

(defun task-prefix-p (prefix target)
  (let ((task (task target)))
    (not (mismatch prefix task
		   :test 'char-equal
		   :end2 (min (length prefix)
			      (length task))))))

(defun next-task (todo)
  (assert (typep todo 'todo))
  (cond ((not (subtask todo))
	 (task todo))
	(t
	 (when-let ((next (next-todo todo)))
	   (task next)))))

(defun next-todo (todo)
  (when (typep todo 'todo)
    (if (subtask todo)
	(do ()
	    ((not (subtask todo)) todo)
	  (setq todo (subtask todo)))
	(next todo))))

(defun locate (str todo)
  (labels ((dfs (node prev)
	     (when (task-prefix-p str node)    
	       (return-from locate (values node prev)))
	     (when-let ((subtask (subtask node)))
	       (multiple-value-bind (node prev) (dfs subtask node)
		 (when node
		   (return-from locate (values node prev)))))
	     (when-let ((next (next node)))
	       (multiple-value-bind (node prev) (dfs next node)
		 (when node
		   (return-from locate (values node prev)))))))
    (dfs todo nil)))

(defvar *todo-pathname* (merge-pathnames
			 (make-pathname :name "todo" :type "obj")
			 #.(or *compile-file-truename* *load-truename*))
  "Path of the file to persist the todo.")

(defvar *todoing* ()
  "The task in progress")

(defclass-autoargs todo ()
  ((task :initform nil)
   (subtask :initform nil)
   (next :initform nil)))

(defun todo-reader (stream char &optional count)
  (declare (ignore count))
  (destructuring-bind (task &optional subtask next)
      (read-delimited-list (closer char)  stream t)
    (make-instance 'todo :task task :subtask subtask :next next)))

(defvar *todo-readtable* (copy-readtable nil))

(defmethod print-object ((o todo) s)
  (cond (*print-readably*
	 (with-slots (task subtask next) o
	   (format s "{~{ ~w~^  ~%~}}"
		   (concatenate 'list
				(list task)
				(when (or next subtask)
				  (list subtask))
				(ensure-list next)))))
	(t
	 (display-todo o :stream s))))

(defun display-todo (todo &key (stream *standard-output*) (depth 0))
  (with-slots (task subtask next) todo
    (terpri stream)
    (loop repeat depth do (format stream " "))
    (format stream "~A" task)
    (finish-output stream)
    (when subtask
      (display-todo subtask :stream stream :depth (+ 2 depth)))
    (finish-output stream)
    (when next
      (display-todo next :stream stream :depth depth))))

(set-macro-character #\{ 'todo-reader () *todo-readtable*)
(set-macro-character #\( 'todo-reader () *todo-readtable*)
(set-macro-character #\} (get-macro-character #\) ()) () *todo-readtable*)
(set-syntax-from-char #\} #\) *todo-readtable* ())
