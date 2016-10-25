;;; ert-runner-reporter-ert+duration.el --- Ert reporter with test duration

;; Copyright (C) 2016 Matúš Goljer

;; Author: Matúš Goljer <matus.goljer@gmail.com>
;; Maintainer: Matúš Goljer <matus.goljer@gmail.com>
;; Version: 0.0.1
;; Created: 25th October 2016

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ert nil 'no-error)

(defvar ert-runner-reporter-ert+duration-test-start-time nil
  "When the last test started.")

(add-hook 'ert-runner-reporter-run-started-functions
          (lambda (stats)
            (ert-runner-message "Running %s tests (%s)\n\n"
                                (length (ert--stats-tests stats))
                                (ert--format-time-iso8601
                                 (ert--stats-start-time stats)))))

(add-hook 'ert-runner-reporter-run-ended-functions
          (lambda (stats abortedp)
            (let ((unexpected (ert-stats-completed-unexpected stats))
                  (expected-failures (ert--stats-failed-expected stats)))
              (ert-runner-message "\n%sRan %s tests, %s results as expected%s (%s)%s\n\n"
                                  (if (not abortedp)
                                      ""
                                    "Aborted: ")
                                  (ert-stats-total stats)
                                  (ert-stats-completed-expected stats)
                                  (if (zerop unexpected)
                                      ""
                                    (format ", %s unexpected" unexpected))
                                  (ert--format-time-iso8601
                                   (ert--stats-end-time stats))
                                  (if (zerop expected-failures)
                                      ""
                                    (format "\n%s expected failures"
                                            expected-failures)))
              (unless (zerop unexpected)
                (ert-runner-message "%s unexpected results:\n" unexpected)
                (cl-loop for test across (ert--stats-tests stats)
                         for result = (ert-test-most-recent-result test) do
                         (when (not (ert-test-result-expected-p test result))
                           (ert-runner-message "%9s  %S\n"
                                               (ert-string-for-test-result
                                                result nil)
                                               (ert-test-name test))))
                (ert-runner-message "\n")))))

(add-hook 'ert-runner-reporter-test-started-functions
          (lambda (_stats _test)
            (setq ert-runner-reporter-ert+duration-test-start-time
                  (current-time))))

(add-hook 'ert-runner-reporter-test-ended-functions
          (lambda (stats test result)
            (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                   (dur (subtract-time
                         (current-time)
                         ert-runner-reporter-ert+duration-test-start-time))
                   (format-string (concat "%9s  %"
                                          (prin1-to-string (length max))
                                          "s/" max "  (% 2.2fs)  %S\n")))
              (ert-runner-message format-string
                                  (ert-string-for-test-result
                                   result
                                   (ert-test-result-expected-p
                                    test result))
                                  (1+ (ert--stats-test-pos stats test))
                                  (float-time dur)
                                  (ert-test-name test)))))

(provide 'ert-runner-reporter-ert+duration)
;;; ert-runner-reporter-ert+duration.el ends here
