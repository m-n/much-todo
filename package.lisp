;;;; package.lisp

(defpackage #:much-todo
  (:use #:cl #:alexandria)
  (:export
   #:todo
   #:focus
   #:unfocus
   #:finish
   #:*todo-pathname*
   #:*todoing*
   #:*todos*
   #:select-todo-list
   #:remove-todo-list
   #:add-todo-list))

