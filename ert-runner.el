;;; ert-runner.el --- TODO

;; Copyright (C) 2013 Johan Andersson

;; Author: Johan Andersson <johan.rejeep@gmail.com>
;; Maintainer: Johan Andersson <johan.rejeep@gmail.com>
;; Version: 0.0.1
;; Keywords: test
;; URL: http://github.com/rejeep/ert-runner.el
;; Package-Requires: ((s "1.6.1") (dash "1.4.0") (f "0.2.0") (commander "0.0.1"))

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

(require 's)
(require 'dash)
(require 'f)
(require 'commander)

(defvar ert-runner-selector t
  "Selector that Ert should run with.")

(defvar ert-runner-load-files nil
  "List of load files.")

(defun ert-runner/pattern (pattern)
  (setq ert-runner-selector pattern))

(defun ert-runner/load (&rest load-files)
  (setq ert-runner-load-files load-files))

(defun ert-runner/usage ()
  (commander-print-usage))

(defun ert-runner/run (&rest tests)
  (let* ((el-tests-fn
          (lambda (file)
            (if tests
                (--any? (s-ends-with? it file) tests)
              (s-matches? "-test\.el$" file))))
         (test-files (f-files (f-expand "test") el-tests-fn))
         (test-helper
          (f-expand "test-helper.el" "test")))
    (-map
     (lambda (load-file)
       (load load-file 'noerror 'nomessage))
     ert-runner-load-files)
    (if (f-exists? test-helper)
        (load test-helper 'noerror 'nomessage))
    (-map
     (lambda (test-file)
       (load test-file 'noerror 'nomessage))
     test-files)
    (ert-run-tests-batch-and-exit ert-runner-selector)))

(commander
 (name "ert-runner")

 (default "help")

 (option "--help, -h" "Show usage information" 'ert-runner/usage)
 (option "-p <pattern>" "Run tests matching pattern" 'ert-runner/pattern)
 (option "-l <*>" "Load files" 'ert-runner/load)

 (command "run [*]" "Run all or specified tests" 'ert-runner/run)
 (command "help" "Show usage information" 'ert-runner/usage))

;;; ert-runner.el ends here
