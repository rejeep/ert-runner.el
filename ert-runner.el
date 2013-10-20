;;; ert-runner.el --- Opinionated Ert testing workflow

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.4.1
;; Keywords: test
;; URL: http://github.com/rejeep/ert-runner.el
;; Package-Requires: ((s "1.6.1") (dash "1.8.0") (f "0.10.0") (commander "0.2.0") (ansi "0.1.0"))

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

;;; Code:

(remove-hook 'find-file-hooks 'vc-find-file-hook)

(require 's)
(require 'dash)
(require 'f)
(require 'commander)
(require 'ansi)

(defvar ert-runner-selector t
  "Selector that Ert should run with.")

(defvar ert-runner-load-files nil
  "List of load files.")

(defvar ert-runner-test-path (f-expand "test")
  "Path to test dir.")

(defconst ert-runner-output-file (getenv "ERT_RUNNER_OUTFILE")
  "Path to outfile used for writing when non script mode.")

(defun ert-runner-print (string)
  (when ert-runner-output-file
    (let ((content (f-read-text ert-runner-output-file 'utf-8)))
      (f-write-text (s-concat content string) 'utf-8 ert-runner-output-file))))

(defadvice princ (after princ-after activate)
  (ert-runner-print (car (ad-get-args 0))))

(defadvice message (after message-after activate)
  (ert-runner-print (s-concat (apply 'format (ad-get-args 0)) "\n")))

(when ert-runner-output-file
  (when (f-file? ert-runner-output-file)
    (f-delete ert-runner-output-file))
  (f-touch ert-runner-output-file))

(defun ert-runner/pattern (pattern)
  (setq ert-runner-selector pattern))

(defun ert-runner/load (&rest load-files)
  (setq ert-runner-load-files load-files))

(defun ert-runner/usage ()
  (commander-print-usage-and-exit))

(defun ert-runner--load (file)
  (if (f-relative? file)
      (setq file (f-expand file)))
  (load file nil :nomessage))

(defun ert-runner/run (&rest tests)
  (unless (f-dir? ert-runner-test-path)
    (error (ansi-red "No test directory. Create one using `ert-runner init`")))
  (let* ((el-tests-fn
          (lambda (file)
            (if tests
                (--any? (s-ends-with? it file) tests)
              (s-matches? "-test\.el$" file))))
         (test-files (f-files (f-expand ert-runner-test-path) el-tests-fn))
         (test-helper
          (f-expand "test-helper.el" ert-runner-test-path)))
    (-each ert-runner-load-files #'ert-runner--load)
    (if (f-exists? test-helper)
        (ert-runner--load test-helper))
    (-each test-files #'ert-runner--load)
    (ert-run-tests-batch-and-exit ert-runner-selector)))

(defun ert-runner/init (&optional name)
  (unless name (setq name (f-filename default-directory)))
  (if (f-dir? "test")
      (error (ansi-red "Directory `test` already exists.")))
  (f-mkdir ert-runner-test-path)
  (f-touch (f-join ert-runner-test-path "test-helper.el"))
  (f-touch (f-join ert-runner-test-path (s-concat name "-test.el")))
  (message "create %s" (ansi-green (f-filename ert-runner-test-path)))
  (message "create  %s" (ansi-green "test-helper.el"))
  (message "create  %s" (ansi-green (s-concat name "-test.el"))))

(defun ert-runner/debug ()
  (setq debug-on-error t)
  (setq debug-on-entry t))

(setq commander-args (-reject 's-blank? (s-split " " (getenv "ERT_RUNNER_ARGS"))))

(commander
 (name "ert-runner")
 (description "Opinionated Ert testing workflow")
 (config ".ert-runner")

 (default ert-runner/run)

 (option "--help, -h" "Show usage information" ert-runner/usage)
 (option "--pattern <pattern>, -p <pattern>" "Run tests matching pattern" ert-runner/pattern)
 (option "--load <*>, -l <*>" "Load files" ert-runner/load)
 (option "--debug" "Enable debug" ert-runner/debug)

 (option "--script" "Run Emacs as a script/batch job (default)" ignore)
 (option "--no-win" "Run Emacs without GUI window" ignore)
 (option "--win" "Run Emacs with full GUI window" ignore)

 (command "init [name]" "Create new test project (optional project name)" ert-runner/init)
 (command "help" "Show usage information" ert-runner/usage))

;;; ert-runner.el ends here
