;;;; utils.lisp

(in-package #:much-todo)

(defmacro with-file-datastructure
    ((symbol pathname &key (readtable *readtable*) &allow-other-keys)
     &body body)
  (with-gensyms (serialize-win stream gpathname)
    `(let ((*readtable* ,readtable)
	   (,gpathname ,pathname)
	   (,serialize-win nil)
	   ,symbol)
       (unwind-protect
	    (prog1 
		(alexandria:with-input-from-file (,stream ,gpathname)
		  (setq ,symbol (read ,stream nil nil nil))
		  ,@body)
	      (alexandria:with-output-to-file (,stream ,gpathname :if-exists :supersede)
		(write ,symbol :stream ,stream)
		(finish-output ,stream))
	      (setq ,serialize-win t))
	 (unless ,serialize-win
	   (error "with-file-datastructure failed to write to ~A" ,gpathname))))))

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

(defun closer (char)
  (ecase char
    (#\( #\))
    (#\{ #\})
    (#\[ #\])
    (#\< #\>)))
