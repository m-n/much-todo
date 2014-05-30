;;;; much-todo.lisp

;;;; preamble

(in-package #:much-todo)

(defvar *todos*
  (list (merge-pathnames
	 (make-pathname :name "test" :type "todo")
	 #-asdf
	 (or *compile-file-truename*
	       *load-truename*
	       *default-pathname-defaults*)
	 #+asdf
	 (asdf:system-source-directory "much-todo")))
  "List of pathnames of todo files.")

(defvar *todo-pathname* (first *todos*)
  "Path of the file to persist the current todo list.")

(defvar *todoing* ()
  "The task in progress")

(defun todo-reader (stream &optional char count)
  (declare (ignore count))
  (flet ((indentation (line)
	   (loop for c across line
		 while (char= #\Space c)
		 count c))
	 (next-line ()
	   (read-line stream nil "" nil)))
    (let* ((top (make-instance 'todo :task (if char
					       (concatenate 'string
							    (string char)
							    (next-line))
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

(defmacro with-todo (var &body body)
  `(if *todo-pathname*
       (with-file-datastructure (,var *todo-pathname* :read-function 'todo-reader)
	 ,@body)
       (format t "No todo file selected.")))

(defclass-autoargs todo ()
  ((task :initform nil)
   (subtask :initform nil)
   (next :initform nil)))

;;;; Interface

(defun todo (&optional new-todo task)
  "Display and return todo from *todo-pathname*, if new-todo given add to the file."
  (let (found unfoundp)
    (with-todo todo
      (when task (or (setq found (locate task todo)) (setq unfoundp t)))
      (cond ((and new-todo task found)
	     (destructuring-bind (at prev first) (car found)
	       (declare (ignorable prev first))
	       (setf (subtask at) (apply 'make-instance 'todo
					 :task new-todo
					 (when (subtask at)
					   (list
					    :next (subtask at)))))))
	    ((and new-todo (not task))
	     (setf todo (make-instance 'todo :task new-todo :next todo)))))
    (cond (unfoundp
	   (format t "~&Task \"~A\" not found -- todo not added.~&" task)
	   (with-todo todo todo))
	  (t (when *todoing* (focus *todoing*))
	     (with-todo todo
	       (display-todo todo)
	       todo)))))


(defun focus (&optional string)
  "Move todo matching string to front of todo file, *todoing* <- next subtask.

If there is no matching string it will add a new task."
  (unless string (setq string ""))
  (let (foundp)
    (with-todo todo
      (when-let ((ordered-todo (nmove-to-front string todo)))
        (setq foundp t)
        (setq todo ordered-todo)
        (setq *todoing* (next-task todo))))
    (unless foundp
      (todo string)
      (focus string))))

(defun unfocus ()
  "Clear *todoing*"
  (setq *todoing* nil))

(defun finish ()
  "Remove current task from the todo list, clear *todoing*."
  (pop-todo)
  (setq *todoing* nil))

(defun add-todo-list (pathname)
  "Add todo list to the todos displayed by select-todo-list."
  (appendf *todos* (ensure-list pathname)))

(defun select-todo-list (&optional number)
  "User-interactive choice between identified todo files."
  (unless *todos* 
    (princ "No pathnames in *todos*.") 
    (return-from select-todo-list))
  (when (not number)
    (format t "~&Choose a todo list:~&")
    (idolist (i todo *todos*)
      (format t "~&~A. ~A~&" (1+ i) (pathname-name todo)))
    (force-output)
    (setq number (read)))
  (if-let (path (when (and (numberp number) (plusp number))
		  ;; todos indexed from 1
		  (decf number)
		  (nth number *todos*)))
    (progn (setq *todo-pathname* path)
           (unfocus)
           (todo))
    (format t "Option ~A is not a number designating one of the todos." number)))

(defun remove-todo-list (&optional number-or-all)
  "Remove todo list from todos displayed by select-todo-list."
  (unless number-or-all
    (format t "~&Choose a todo to remove:~&")
    (idolist (i todo *todos*) 
      (format t "~&~A. ~A~&" (1+ i) (pathname-name todo)))
    (force-output)
    (setq number-or-all (read)))
  (cond ((numberp number-or-all)
	 (decf number-or-all)
	 (assert (and (integerp number-or-all) (< -1 number-or-all (length *todos*)))
		 (number-or-all) "Not one of the integer options presented.")
	 (when (equalp *todo-pathname* (nth number-or-all *todos*))
	   (setq *todo-pathname* nil
		 *todoing* nil))
	 (setq *todos* (append (subseq *todos* 0 number-or-all)
			       (subseq *todos* (1+ number-or-all)))))
	(t (setq *todos* nil
		 *todo-pathname* nil
		 *todoing* nil))))

;;;; Support

(defun pop-todo ()
  "Remove the first todo from the list."
  (with-todo todo
    (let ((top todo)
	  (next (first-todo todo)))
      (if (subtask todo)
	  (do ((subtask (subtask todo) (subtask subtask))
	       (previous todo subtask))
	      ((not (subtask subtask))
	       (setf (subtask previous) (next next))
	       (format t "~&Finished:~&~A" (task next))))
	  (progn
	    (setf todo (next todo))
	    (format t "~&Finished:~&~A" (task top)))))))

(defun nmove-to-front (string todo)
  "If string is a prefix for the task of a todo, move it to the front of todo."
  (when (task-prefix-p string todo)
    (return-from nmove-to-front todo))
  (let ((stack (locate string todo)))
    (unless stack (return-from nmove-to-front nil))
    (mapl (lambda (more)
	    (destructuring-bind (looking prev first) (car more)
	      (when prev
		(setf (next prev) (next looking)
		      (next looking) first))
	      (unless (cdr more) (return-from nmove-to-front looking))
	      (setf (subtask (first (cadr more))) looking)))
	  stack)))

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
	 (when-let ((next (first-todo todo)))
	   (task next)))))

(defun first-todo (todo)
  (assert (typep todo 'todo))
  (do ()
      ((not (subtask todo)) todo)
    (setq todo (subtask todo))))

(defun locate (string todo)
  (labels ((dfs (stack)
	     (destructuring-bind (node prev first) (car stack)
	       (declare (ignorable prev))
	       (when (task-prefix-p string node)
		 (return-from locate stack))
	       (when-let ((subtask (subtask node)))
		 (dfs (cons (list subtask nil subtask) stack)))
	       (when-let ((next (next node)))
		 (dfs (cons (list next node first) (cdr stack)))))))
    (dfs (cons (list todo nil todo) nil))))

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

