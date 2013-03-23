;;;; much-todo.asd

(asdf:defsystem #:much-todo
  :serial t
  :description "A todo list for the cl repl"
  :author "Matt Niemeir <matt.niemeir@gmail.com>"
  :license "BSD 2-clause"
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "utils")
               (:file "much-todo")))

