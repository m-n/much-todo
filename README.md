much-todo
=========

A todo list for the cl repl.

Warning: alpha quality. There may still be some situations where this
deletes your todo file without warning.

This package creates and manages a simple todo list, transparently backed by
a file, designed for use at the slime repl. It is tested in SBCL but purports
to be written in portable Common Lisp.

Example
-------

    ; SLIME 2013-03-12
    ;(in-package much-todo) 
    (todo)

    First task
      Subtask of first task
    Second Task
    Third Task

    #<TODO "First task" {1008EF5823}>
    ;(in-package much-todo) 
    (todo "zeroth task")

    zeroth task
    First task
      Subtask of first task
    Second Task
    Third Task

    #<TODO "zeroth task" {1009037E03}>
    ;(in-package much-todo) 
    (todo "Subtask of third" "third")

    zeroth task
    First task
      Subtask of first task
    Second Task
    Third Task
      Subtask of third

    #<TODO "zeroth task" {10090A3E13}>
    ;(in-package much-todo) 
    (focus "third")

    "Subtask of third"
    ;(in-package much-todo) subtask of third
    (todo)

    Third Task
      Subtask of third
    zeroth task
    First task
      Subtask of first task
    Second Task

    #<TODO "Third Task" {10091FE663}>
    ;(in-package much-todo) subtask of third
    (finish)

    Finished:
    Subtask of third

    NIL
    ;(in-package much-todo) 
    (todo)

    Third Task
    zeroth task
    First task
      Subtask of first task
    Second Task

    #<TODO "Third Task" {10092C9BE3}>
    ;(in-package much-todo) 
    
Interface
---------

(TODO &optional new-todo task) > function  
 Return the todo from *todo-pathname*, add the new-todo if present to the file.

(FOCUS &optional string) > function  
 Move todo matching string to front of todo file, *todoing* <- next subtask.

(FINISH) > function  
 Clear *todoing*, remove the most recent task.
Fixme: doesn't insure that the task named in *todoing* is still the most 
recent task.

(UNFOCUS) > function  
 Clear *todoing*

*todoing* > variable  
 The task in progress

*todo-pathname* > variable  
 Path of the file to persist the todo.

Representation of the todo file
-------------------------------

Each line of the todo file represents one task. If a line is indented by two
spaces more than the previous line, it is a subtask of the previous line.

slime-repl integration
----------------------

The following function is adjusted from the slime sources. It is an elisp
function which determines the display of slime's prompt. If you replace slime's
version of this version of the function and push :much-todo to *features* in 
your lisp image, the slime prompt will display as in the example above.

Known Issue: Occasionally this seems to be hanging my prompt, requiring a
C-c C-c or C-g C-g to unhang it.

    (defun slime-repl-insert-prompt ()
      "Insert the prompt (before markers!).
    Set point after the prompt.
    Return the position of the prompt beginning.
        
        If `slime-repl-suppress-prompt' is true, does nothing and returns nil."
      (goto-char slime-repl-input-start-mark)
      (if slime-repl-suppress-prompt
          (unless (bolp) (insert-before-markers "\n"))
        (slime-save-marker slime-output-start
          (slime-save-marker slime-output-end
            (unless (bolp) (insert-before-markers "\n"))
            (let ((prompt-start (point))
                  (prompt  (downcase (format ";(in-package %s) %s\n"
                                             (slime-lisp-package-prompt-string)
                                             (slime-eval
                                              '(cl:eval (cl:read-from-string
                                                         "(cl:or #+much-todo much-todo::*todoing*
                                                                   \"\")")))))))
              (slime-propertize-region
                  '(face slime-repl-prompt-face read-only t intangible t
                         slime-repl-prompt t
                         ;; emacs stuff
                         rear-nonsticky (slime-repl-prompt read-only face intangible)
                         ;; xemacs stuff
                         start-open t end-open t)
                (insert-before-markers prompt))
              (set-marker slime-repl-prompt-start-mark prompt-start)
              prompt-start)))))
