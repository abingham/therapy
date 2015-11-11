# therapy

*"This is supposed to be torture, not therapy!"  -- Minerva Mayflower*

`therapy` is fundamentally a set of hooks which get executed when the major
version of your configured Python interpreter changes. If you regularly need to
switch between Python major version in an Emacs session, this is for you!

A typical situation where `therapy` can come in handy is if you've got parallel
Python2 and Python3 system installations. In this case, certain tools like
`flake8` and `ipython` can have different names depending on the major version
of Python you're using: `flake8` vs. `flake8-3` and `ipython` vs. `ipython3`,
respectively. Since Emacs can use these tools, you need to be able to tell Emacs
which commands to use in which situations. `therapy` gives you hooks for doing
just this.

The basic approach is that you add hooks to the `therapy-python2-hooks` and
`therapy-python3-hooks` lists. Your hooks will be called when `therapy` detects
that the Python major version has changed, and your hook functions can do things
like set the `flake8` command, update the `python-shell-interpreter` variable,
and so forth.

**But how does `therapy` know when the Python major version has changed?** It
doesn't, really. You have to tell it by calling `therapy-interpreter-changed`.
This tells therapy to run the hooks; it will detect the configured major version
and run the appropriate hooks. Alternatively, you can call
`therapy-set-python-interpreter` which a) sets `python-shell-interpreter` and b)
calls the hooks.

## For example..
Here's how you can add hooks that update the `flake8` command
for major version switches.
```Emacs Lisp
;; use flake8 for python2
(add-hook 'therapy-python2-hook
    (lambda () (setq flycheck-python-pyflakes-executable "flake8"))

;; use flake8-3 for python3
(add-hook 'therapy-python3-hook
    (lambda () (setq flycheck-python-pyflakes-executable "flake8-3"))
```

Now if you want to switch to Python3 and run the hooks, you can do this:
```Emacs Lisp
(therapy-set-python-interpreter "/path/to/new/python")
```

`therapy` will figure out major version of your Python interpreter and call the
right hooks.

## Interacting with pyvenv

If you use [`pyvenv`](https://github.com/jorgenschaefer/pyvenv) to work with
virtual environments, you might want to have `therapy` run its hooks whenever
the virtual environment changes. You can do this easily with
`pyvenv-post-activate-hooks`:
```Emacs Lisp
(defun my-pyvenv-hook ()
  "Hook for when we change virtual environments."

  ;; Do this so that we're sure to pick up the venv's interpreter.
  (therapy-set-python-interpreter (executable-find "python"))

  ;; Activate the right toolset based on the detected major version.
  (therapy-interpreter-changed))

(add-hook 'pyvenv-post-activate-hooks
    'my-pyvenv-hook)
```

Now, whenever the virtual environment is switched, therapy will get to run its
hook.
