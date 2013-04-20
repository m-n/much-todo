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
    (display-todo todo)
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
	  (do ((subtask (subtask todo) (subtask subtask))
	       (previous todo subtask))
	      ((not (subtask subtask))
	       (setf (subtask previous) (next next))
	       (format t "~&Finished:~&~A" (task next))))
	  (progn 
	    (setf todo (next todo)
		  (next todo) nil)
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
  (flet ((indentation (line)
	   (loop for c across line
		 while (char= #\Space c)
		 count c))
	 (next-line ()
	   (read-line stream nil "" nil)))
    (let* ((top (make-instance 'todo :task (concatenate 'string
							(string char)
							(next-line))))
	   (stack (list top)))
      (do* ((line (next-line) (next-line))
	    (previous-indentation 0 indentation)
	    (indentation (indentation line)
			 (indentation line))
	    (task (subseq line indentation)
		  (subseq line indentation)))
	   ((string= "" line) top)
	(let ((new (make-instance 'todo :task task)))
	  (cond ((<= indentation previous-indentation)
		 (loop repeat (- (length stack) (/ indentation 2) 1)
		       do (pop stack))
		 (setf (next (car stack)) new
		       (car stack)  new))
		((> indentation previous-indentation)
		 (assert (= (- indentation previous-indentation) 2)
			 () "Malformed todo")
		 (setf (subtask (car stack)) new)
		 (push new stack))))))))

(defvar *todo-readtable*
  (prog1-let (rt (copy-readtable nil))
    (dotimes (i ;char-code-limit ;; causes error in ccl
	      256)
      (set-macro-character (code-char i) 'todo-reader () rt))))

(defmethod print-object ((o todo) s)
  (if *print-readably*
      (display-todo o :stream s)
      (print-unreadable-object (o s :type t :identity t)
	(if (> (length (task o)) 11)
	    (format s "~S..." (subseq (task o) 0 8))
	    (format s "~S" (task o))))))

(defun display-todo (todo &key (stream *standard-output*) (depth 0))
  (with-slots (task subtask next) todo
    (loop repeat depth do (format stream " "))
    (format stream "~A" task)
    (terpri stream)
    (finish-output stream)
    (when subtask
      (display-todo subtask :stream stream :depth (+ 2 depth)))
    (finish-output stream)
    (when next
      (display-todo next :stream stream :depth depth))))

