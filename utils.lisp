;;;; utils.lisp

(in-package #:much-todo)

(defmacro with-file-datastructure
    ((symbol pathname &key (readtable '*readtable*)
			   (read-function ())
		      &allow-other-keys)
     &body body)
  (with-gensyms (write-win read-win stream gpathname gread-function backup)
    `(let ((*readtable* ,readtable)
	   (,gpathname ,pathname)
	   (,gread-function ,read-function)
	   ,symbol ,backup ,write-win ,read-win)
       (unwind-protect
	    (prog1 
		(alexandria:with-input-from-file (,stream ,gpathname)
		  (setq ,symbol (if ,gread-function
				    (funcall ,gread-function ,stream)
				    (read ,stream nil nil nil)))
		  ,@body)
	      (setq ,read-win t)
	      (setq ,backup (read-file-into-string ,gpathname))
	      (alexandria:with-output-to-file (,stream ,gpathname :if-exists :supersede)
		(with-standard-io-syntax (write ,symbol :stream ,stream))
		(finish-output ,stream)
		(setq ,write-win t)))
	 (unless (and ,write-win ,read-win)
	   (when ,read-win
	     (write-string-into-file ,backup ,gpathname :if-exists :supersede))
	   (error "with-file-datastructure failed to write to ~A" ,gpathname))))))

(defmacro prog1-let ((var val) &body body)
  `(let ((,var ,val))
     (prog1 ,var ,@body)))

(defmacro idolist ((index-var value-var list &optional return) &body body)
  "Indexed dolist"
  `(loop for ,index-var from 0
	 for ,value-var in ,list
	 do (tagbody ,@body)
	 finally (return ,return)))

(defmacro defclass-autoargs (name superclasses slots &rest options)
  "Produces accessors and initargs named for the slot name, unless
:accesssor, :reader, or :writer is included in the slot (for :accessor)
or :initarg is included in the slot (for :initarg)
Slots can be symbols or lists including further args."
  `(defclass ,name ,superclasses
     ,(loop for (slot . args) in (mapcar #'ensure-list slots)
	    collect (append (append (list slot)
				    (unless (intersection
					     '(:accessor :reader :writer)
					     args)
				      (list :accessor slot))
				    (unless (member :initarg args)
				      (list :initarg
					    (intern (symbol-name slot)
						    :keyword))))
			    args))
     ,@options))

