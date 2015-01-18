;;; therapy.el --- Useful Python stuff
;;
;; Copyright (c) 2015 Austin Bingham
;;
;; Author: Austin Bingham <austin.bingham@gmail.com>
;; Version: 0.1
;; URL: https://github.com/abingham/therapy
;; Package-Requires: ((emacs "24") (dash "2.10.0") (f "0.17.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; "This is supposed to be torture, not therapy!"  -- Minerva Mayflower
;;
;; Description:
;;
;; therapy is a suite of functionality that improves Python
;; development in Emacs.
;;
;;; License:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Code:

(require 'dash)
(require 'f)
(require 'python)

(defgroup therapy nil
  "Useful Python stuff."
  :group 'tools
  :group 'programming)

(defcustom therapy-python2-hooks nil
  "Hook for when Python 2 is activated."
  :group 'therapy
  :type 'hook)

(defcustom therapy-python3-hooks nil
  "Hook for when Python 3 is activated."
  :group 'therapy
  :type 'hook)

(defvar therapy-test-command "python -m nose"
  "The command used to execute tests from the project root.")

(defun therapy-interpreter-changed ()
  "Call this when the Python interpreter is changed.

This will run the correct hooks for the new version."
  (if (string-equal "3" (therapy-python-major-version python-shell-interpreter))
      (run-hooks 'therapy-python3-hooks)
    (run-hooks 'therapy-python2-hooks)))

(defun therapy-python-major-version (interpreter)
  "Find major version of INTERPRETER, a Python interpreter command."
  (let* ((program "\"import sys; print(sys.version_info.major)\"")
         (command (format "%s -c %s" interpreter program))
         (results (shell-command-to-string command)))
    ;; The output has the major version in the firt character.
    (substring results 0 1)))

(defun therapy-find-project-root (start-dir)
  "Find the likely Python project root for START-DIR.

Since there's no strict definition of a Python project, this
makes a few educated guesses.  It first looks for a directory
containing setup.py.  If it finds one, it assumes that this is
the root.  Otherwise, it looks for the first parent directory
that doesn't contain __init__.py which, if found, is the project
root.

This may get expanded and/or modified in the future."
  (or (therapy--find-setup-py start-dir)
      (therapy--find-no-init start-dir)))

;; Testing tools

(defun therapy-run-all-tests ()
  "Find all unit-tests for the current package and run them."
  (interactive)

  (unless (buffer-file-name)
    (error
     (format "No directory associated with the buffer %s. Unable to search for project root."
             (current-buffer))))

  (let* ((buffer-dir (f-dirname (buffer-file-name)))
         (root-dir (therapy-find-project-root buffer-dir)))
    (unless root-dir (error "No project root detected!"))
    (let ((default-directory (file-name-as-directory root-dir)))
      (compile therapy-test-command))))

;; Utility/infrastructure

(defun therapy--parent-dirs (dir)
  "Find all parent directories of DIR.

Returns a list of directories.  If DIR is not nil then the first
entry in the list is DIR."
  (if dir
    (cons dir (therapy--parent-dirs (f-dirname dir)))))

(defun therapy--find-setup-py (dir)
  "Search DIR and its parents for setup.py.

Returns the first directory containing setup.py, or nil."
  (-first
   (lambda (dir) (f-exists? (f-join dir "setup.py")))
   (therapy--parent-dirs dir)))

(defun therapy--find-no-init (dir)
  "Search DIR and its parents for directories without __init__.py.

Returns the first directory not containing __init__.py, or nil."
  (-first
   (lambda (dir) (not (f-exists? (f-join dir "__init__.py"))))
   (therapy--parent-dirs dir)))

(provide 'therapy)

;;; therapy.el ends here
