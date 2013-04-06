much-todo
=========

A todo list for the cl repl

This package creates and manages a simple todo list, transparently backed by
a file, designed for use at the slime repl.

Example
-------

    ;(in-package much-todo) 
    (todo)
    
    First task
      Subtask of first task
    Second Task
    Third Task
    ;(in-package much-todo) 
    (todo "subtask of third" "third")
    
    First task
      Subtask of first task
    Second Task
    Third Task
      subtask of third
    ;(in-package much-todo) 
    (focus "third")
    "subtask of third"
    ;(in-package much-todo) subtask of third
    (todo)
    
    Third Task
      subtask of third
    First task
      Subtask of first task
    Second Task
    ;(in-package much-todo) subtask of third
    (finish)
    Finished:
    subtask of third
    NIL
    ;(in-package much-todo) 
    (todo)
    
    Third Task
    First task
      Subtask of first task
    Second Task
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

slime-repl integration
----------------------

The following function is adjusted from the slime sources. It is an elisp
function which determines the display of slime's prompt. If you replace slime's
version of this version of the function and push :much-todo to *features* in 
your lisp image, the slime prompt will display as in the example above.

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
