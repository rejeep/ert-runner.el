;;; ert-runner.el --- Opinionated Ert testing workflow

;; Copyright (C) 2013, 2014 Johan Andersson
;; Copyright (C) 2013 Sebastian Wiesner <swiesner@lunaryorn.com>
;; Copyright (C) 2013 Jorgen Schaefer <contact@jorgenschaefer.de>

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.7.0
;; Keywords: test
;; URL: http://github.com/rejeep/ert-runner.el
;; Package-Requires: ((s "1.6.1") (dash "1.8.0") (f "0.10.0") (commander "0.2.0") (ansi "0.1.0") (shut-up "0.1.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Opinionated Ert testing workflow.

;;; Code:

(require 'cl-lib)
(require 's)
(require 'dash)
(require 'f)
(require 'commander)
(require 'ansi)
(require 'shut-up)
(eval-and-compile
  (unless (require 'ert nil 'no-error)
    (load "ert-compat" nil 'no-message)))

(when noninteractive
  (shut-up-silence-emacs))

(defvar ert-runner-selector '(and t)
  "Selector that Ert should run with.")

(defvar ert-runner-load-files nil
  "List of load files.")

(defvar ert-runner-test-path (f-expand "test")
  "Path to test dir.")

(defvar ert-runner-verbose t
  "If true, show all message output, otherwise hide.")

(defvar ert-runner-profile nil
  "If true, show profiling output, otherwise hide.")

(defvar ert-runner-output-buffer "*ert-runner outout*"
  "The buffer in which test output is stored in case it is
needed by a reporter later.")

(defvar ert-runner-reporter-name "dot"
  "The reporter to use.")

(defvar ert-runner-reporter-run-started-functions nil
  "Functions run when a test run starts, before any test is run.

Arguments: stats")

(defvar ert-runner-reporter-run-ended-functions nil
  "Functions run after all tests have run.

Arguments: stats, abortedp")

(defvar ert-runner-reporter-test-started-functions nil
  "Functions run before every test.

Arguments: stats, test")

(defvar ert-runner-reporter-test-ended-functions nil
  "Functions run after each test.

Arguments: stats, test, result")

(defconst ert-runner-reporters-path
  (f-expand "reporters" (f-dirname (f-this-file)))
  "Path to directors directory.")

(defconst ert-runner-output-file (-when-let (env (getenv "ERT_RUNNER_OUTFILE"))
                                   (f-expand env))
  "Path to outfile used for writing when non script mode.")

(when ert-runner-output-file
  (when (f-file? ert-runner-output-file)
    (f-delete ert-runner-output-file))
  (f-touch ert-runner-output-file))

;; Retain the original message function for later use
(fset 'ert-runner/princ-original (symbol-function 'message))
(defun ert-runner-message (format &rest args)
  "Emit a formatted message.

This bypasses the normal output capturing ert-runner does, and is
primarily intended for reporters."
  (let ((message (apply #'format format args)))
    (if ert-runner-output-file
        (f-append-text message 'utf-8 ert-runner-output-file)
      (princ message t))))

;; Work around Emacs bug #16121, which is fixed in Emacs 24.4, but still present
;; in former releases. See ;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16121
(when (version< emacs-version "24.4")
  ;; Remember the original definition
  (declare-function ert-runner/ert-select-tests "ert-runner"
                    (selector universe))
  (fset 'ert-runner/ert-select-tests (symbol-function 'ert-select-tests))
  ;; And fix handling of string selectors
  (fset 'ert-select-tests
        (lambda (selector universe)
          (if (stringp selector)
              (cl-etypecase universe
                ((member t) (mapcar #'ert-get-test
                                    (apropos-internal selector #'ert-test-boundp)))
                (list (cl-remove-if-not
                       (lambda (test)
                         (and (ert-test-name test)
                              (string-match selector
                                            (symbol-name (ert-test-name test)))))
                       universe)))
            (ert-runner/ert-select-tests selector universe)))))

(defun ert-runner/add-selector (selector)
  (add-to-list 'ert-runner-selector selector 'append))

(defun ert-runner/make-tag-selector (tag)
  (let* ((tag-symbol (intern (s-chop-prefix "!" tag)))
         (tag-selector `(tag ,tag-symbol)))
    (if (s-starts-with? "!" tag)
        `(not ,tag-selector)
      tag-selector)))

(defun ert-runner/tags (tags)
  "Run tests matching tags."
  (let* ((tag-list (s-split "," tags 'omit-nulls))
         (selectors (-map #'ert-runner/make-tag-selector tag-list)))
    (ert-runner/add-selector `(or ,@selectors))))

(defun ert-runner/pattern (pattern)
  "Run tests matching PATTERN."
  (ert-runner/add-selector pattern))

(defun ert-runner/load (&rest load-files)
  "Load LOAD-FILES."
  (setq ert-runner-load-files load-files))

(defun ert-runner/load-path (path)
  "Append PATH to `load-path'."
  (add-to-list 'load-path path))

(defun ert-runner/usage ()
  "Show usage information."
  (commander-print-usage-and-exit))

(defun ert-runner--load (file)
  (if (f-relative? file)
      (setq file (f-expand file)))
  (load file nil :nomessage))

(defun ert-runner--expand-test-path (path)
  "Build expanded list of test files from PATH.
Paths to files will simply be expanded, whereas paths to
directories will be recursively checked for \"*-test.el\" files.
An error will be signaled if a named file does not exist."
  (setq path (f-expand path))
  (unless (f-exists? path)
    (error "%s" (ansi-red (format "`%s` does not exist." path))))
  (if (f-dir? path)
      (f-files path
               (lambda (file)
                 (s-matches? "-test\.el$" file))
               t)
    path))

(defun ert-runner--test-files (paths)
  "Expand PATHS into a list of test files to run.
See `ert-runner--expand-test-path' for details.  If PATHS is
nil, `ert-runner-test-path' will be used instead."
  (unless paths
    (if (f-dir? ert-runner-test-path)
        (setq paths (list ert-runner-test-path))
      (error "%s" (ansi-red "No test directory. Create one using `ert-runner init`."))))
  (-flatten (mapcar #'ert-runner--expand-test-path paths)))

(defun ert-runner/run (&rest tests)
  (ert-runner/use-reporter ert-runner-reporter-name)
  (let ((test-files (ert-runner--test-files tests))
        (test-helper (f-expand "test-helper.el" ert-runner-test-path)))
    (condition-case e
        (progn
          (-each ert-runner-load-files #'ert-runner--load)
          (if (f-exists? test-helper)
              (ert-runner--load test-helper))
          (-each test-files #'ert-runner--load))
      (error
       (ert-runner-message "Error during test setup: %S. No tests were run.\n" e)
       (kill-emacs 1)))
    (if ert-runner-verbose
        (ert-runner/run-tests-batch-and-exit ert-runner-selector)
      (shut-up
        (ert-runner/run-tests-batch-and-exit ert-runner-selector)))))

(defun ert-runner/init (&optional name)
  "Create new test project (optional project name)."
  (unless name (setq name (f-filename default-directory)))
  (if (f-dir? "test")
      (error "%s" (ansi-red "Directory `test` already exists.")))
  (message "create %s" (ansi-green (f-filename ert-runner-test-path)))
  (f-mkdir ert-runner-test-path)
  (message "create  %s" (ansi-green "test-helper.el"))
  (let ((test-file (s-concat name "-test.el")))
    (with-temp-file (f-join ert-runner-test-path "test-helper.el")
      (insert (format "\
;;; test-helper.el --- Helpers for %s

;;; test-helper.el ends here
" test-file)))
    (message "create  %s" (ansi-green (s-concat name "-test.el")))
    (with-temp-file (f-join ert-runner-test-path test-file)
      (insert (format "\
;;; %s --- Tests for %s

;;; %s ends here
" test-file name test-file)))))

(defun ert-runner/debug ()
  "Enable debug."
  (setq debug-on-error t))

(defun ert-runner/verbose ()
  "Show package output."
  (setq ert-runner-verbose t))

(defun ert-runner/profile ()
  "Show profiling output."
  (setq ert-runner-profile t))

(defun ert-runner/quiet ()
  "Do not show package output."
  (when noninteractive
    (setq ert-runner-verbose nil)))

(defun ert-runner/set-reporter (name)
  "Set the reporter (default: dot)."
  (setq ert-runner-reporter-name name))

(defun ert-runner/reporters ()
  "List available reporters."
  (-map
   (lambda (file)
     (message (s-chop-prefix "ert-runner-reporter-" (f-no-ext (f-filename file)))))
   (f-files ert-runner-reporters-path (lambda (file) (equal (f-ext file) "el"))))
  (kill-emacs 0))

(defun ert-runner/use-reporter (name)
  (let ((reporter-lib-name (format "ert-runner-reporter-%s" name)))
    (when (not (require (intern reporter-lib-name)
                        (f-expand reporter-lib-name
                                  ert-runner-reporters-path)
                        t))
      (error (ansi-red (format "Invalid reporter %s, list available with --reporters" name))))))

(defun ert-runner/run-tests-batch-and-exit (selector)
  "Run tests in SELECTOR and exit Emacs."
  (when ert-runner-profile
    (profiler-start 'cpu))
  (let ((stats (ert-runner/run-tests-batch selector)))
    (when ert-runner-profile
      (profiler-report)
      (profiler-report-write-profile "ert-profile")
      (profiler-stop)
      (message "Profile saved as to file ert-profile")
      (message "Use `profiler-find-profile' to view"))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

(defun ert-runner/run-tests-batch (selector)
  "Run tests in SELECTOR, calling reporters for updates."
  (unless selector (setq selector 't))
  (ert-run-tests
   selector
   (lambda (event-type &rest event-args)
     (cl-ecase event-type
       (run-started
        (cl-destructuring-bind (stats) event-args
          (run-hook-with-args 'ert-runner-reporter-run-started-functions
                              stats)))
       (run-ended
        (cl-destructuring-bind (stats abortedp) event-args
          (run-hook-with-args 'ert-runner-reporter-run-ended-functions
                              stats abortedp)))
       (test-started
        (with-current-buffer (get-buffer-create ert-runner-output-buffer)
          (erase-buffer))
        (cl-destructuring-bind (stats test) event-args
          (run-hook-with-args 'ert-runner-reporter-test-started-functions
                              stats test)))
       (test-ended
        (cl-destructuring-bind (stats test result) event-args
          (unless (ert-test-result-expected-p test result)
            (cl-etypecase result
              (ert-test-passed
               (ert-runner-message "Test %S passed unexpectedly\n"
                                   (ert-test-name test)))
              (ert-test-result-with-condition
               (ert-runner-message "Test %S backtrace:\n\n"
                                   (ert-test-name test))
               (with-temp-buffer
                 (let ((backtrace
                        (ert-test-result-with-condition-backtrace result)))
                   (if (> emacs-major-version 26)
                       (backtrace-to-string backtrace)
                     ;; The signature of ‘ert--print-backtrace’ has changed in
                     ;; Emacs 26: it now takes two arguments.  We try to call
                     ;; it with the new calling convention first and fall back
                     ;; to the old one in case of error.  FIXME: We shouldn’t
                     ;; be using internal functions from ert.el in the first
                     ;; place.
                     (condition-case nil
                         (ert--print-backtrace backtrace nil)
                       (wrong-number-of-arguments
                        (ert--print-backtrace backtrace)))))
                 (goto-char (point-min))
                 (while (not (eobp))
                   (let ((start (point))
                         (end (progn (end-of-line) (point))))
                     (setq end (min end
                                    (+ start
                                       ert-batch-backtrace-right-margin)))
                     (ert-runner-message "%s\n" (buffer-substring-no-properties
                                                 start end)))
                   (forward-line 1))
                 (ert-runner-message "\n"))
               (with-temp-buffer
                 (ert--insert-infos result)
                 (insert "    ")
                 (let ((print-escape-newlines t)
                       (print-level 5)
                       (print-length 10))
                   (ert--pp-with-indentation-and-newline
                    (ert-test-result-with-condition-condition result)))
                 (ert-runner-message "Test %S condition:\n\n"
                                     (ert-test-name test))
                 (ert-runner-message "%s\n" (buffer-string))))
              (ert-test-aborted-with-non-local-exit
               (ert-runner-message "Test %S aborted with non-local exit\n"
                                   (ert-test-name test)))
              (ert-test-quit
               (ert-runner-message "Quit during %S\n" (ert-test-name test))))
            (with-current-buffer (get-buffer-create ert-runner-output-buffer)
              (when (not (= (point-min) (point-max)))
                (ert-runner-message "Test %S output:\n\n"
                                    (ert-test-name test))
                (ert-runner-message "%s" (buffer-string))
                (ert-runner-message "\n"))))
          (run-hook-with-args 'ert-runner-reporter-test-ended-functions
                              stats test result)))))))

(-when-let (args (getenv "ERT_RUNNER_ARGS"))
  (setq commander-args (-reject 's-blank? (s-split " " args))))

(commander
 (name "ert-runner")
 (description "Opinionated Ert testing workflow")
 (config ".ert-runner")

 (default ert-runner/run)

 (option "--help, -h" ert-runner/usage)
 (option "--pattern <pattern>, -p <pattern>" ert-runner/pattern)
 (option "--tags <tags>, -t <tags>" ert-runner/tags)
 (option "--load <*>, -l <*>" ert-runner/load)
 (option "--debug" ert-runner/debug)
 (option "--quiet" ert-runner/quiet)
 (option "--verbose" ert-runner/verbose)
 (option "--profile" ert-runner/profile)
 (option "--reporter <name>" ert-runner/set-reporter)
 (option "--reporters" ert-runner/reporters)
 (option "-L <path>" ert-runner/load-path)

 (option "--script" "Run Emacs as a script/batch job (default)" ignore)
 (option "--no-win" "Run Emacs without GUI window" ignore)
 (option "--win" "Run Emacs with full GUI window" ignore)

 (command "init [name]" ert-runner/init)
 (command "help" ert-runner/usage))

;;; ert-runner.el ends here
