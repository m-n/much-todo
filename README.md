much-todo
=========

A todo list for the cl repl.

This package creates and manages simple todo lists backed by files,
 designed for use at the slime repl. It is tested in SBCL but purports
to be written in portable Common Lisp.

Example
-------

_Display_ the current todo

    ; SLIME 2013-03-12
    ;(in-package much-todo) 
    (todo)

    First task
      Subtask of first task
    Second Task
    Third Task

    #<TODO "First task" {1008EF5823}>

_Add_ a task
    
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

_Focus on (select)_ a task
    
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

_Finish (remove)_ a task

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
    
_Add_ a todo list

    ;(in-package much-todo) 
    (add-todo-list #P"/home/me/cl/much-todo/much-todo.todo")

    (#P"/home/me/cl/much-todo/test.todo"
     #P"/home/me/cl/much-todo/much-todo.todo")

_Select_ a todo list

    ;(in-package much-todo) 
    (select-todo-list)

    Choose a todo list:
    1. test
    2. much-todo
    1
    first
      sub of first
        next of sub
    second
      sub of second

    #<TODO "first" {10068808D3}>

_Remove_ the todo lists (doesn't delete the files)

    ;(in-package much-todo) 
    (remove-todo-list t)

    NIL

Interface
---------

    (TODO &optional new-todo task) > function
     Display and return todo from *todo-pathname*, if new-todo given add to the file.
    
    (FOCUS &optional string) > function
     Move todo matching string to front of todo file, *todoing* <- next subtask.
    
    (FINISH) > function
     Remove current task from the todo list, clear *todoing*.
    
    (UNFOCUS) > function
     Clear *todoing*
    
    (ADD-TODO-LIST pathname) > function
     Add todo list to the todos displayed by select-todo-list.
    
    (SELECT-TODO-LIST &optional number) > function
     User-interactive choice between identified todo files.

    (REMOVE-TODO-LIST &optional number-or-all) > function
     Remove todo list from todos displayed by select-todo-list.
    
    *todos* > variable
     List of pathnames of todo files.
    
    *todo-pathname* > variable
     Path of the file to persist the current todo list.
    
    *todoing* > variable
     The task in progress
    
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
