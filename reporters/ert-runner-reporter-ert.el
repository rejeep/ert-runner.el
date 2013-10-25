;;; ert-runner-reporter-ert.el --- ERT-style reporter

;; Most of the code here is taken directly from ert.el's
;; `ert-run-tests-batch' function.

(require 'ert)

(add-hook 'ert-runner-reporter-run-started-functions
          (lambda (stats)
            (message "Running %s tests (%s)"
                     (length (ert--stats-tests stats))
                     (ert--format-time-iso8601
                      (ert--stats-start-time stats)))))

(add-hook 'ert-runner-reporter-run-ended-functions
          (lambda (stats abortedp)
            (let ((unexpected (ert-stats-completed-unexpected stats))
                  (expected-failures (ert--stats-failed-expected stats)))
              (message "\n%sRan %s tests, %s results as expected%s (%s)%s\n"
                       (if (not abortedp)
                           ""
                         "Aborted: ")
                       (ert-stats-total stats)
                       (ert-stats-completed-expected stats)
                       (if (zerop unexpected)
                           ""
                         (format ", %s unexpected" unexpected))
                       (ert--format-time-iso8601 (ert--stats-end-time stats))
                       (if (zerop expected-failures)
                           ""
                         (format "\n%s expected failures" expected-failures)))
              (unless (zerop unexpected)
                (message "%s unexpected results:" unexpected)
                (cl-loop for test across (ert--stats-tests stats)
                         for result = (ert-test-most-recent-result test) do
                         (when (not (ert-test-result-expected-p test result))
                           (message "%9s  %S"
                                    (ert-string-for-test-result result nil)
                                    (ert-test-name test))))
                (message "%s" "")))))

(add-hook 'ert-runner-reporter-test-ended-functions
          (lambda (stats test result)
            (unless (ert-test-result-expected-p test result)
              (cl-etypecase result
                (ert-test-passed
                 (message "Test %S passed unexpectedly" (ert-test-name test)))
                (ert-test-result-with-condition
                 (message "Test %S backtrace:" (ert-test-name test))
                 (with-temp-buffer
                   (ert--print-backtrace (ert-test-result-with-condition-backtrace
                                          result))
                   (goto-char (point-min))
                   (while (not (eobp))
                     (let ((start (point))
                           (end (progn (end-of-line) (point))))
                       (setq end (min end
                                      (+ start ert-batch-backtrace-right-margin)))
                       (message "%s" (buffer-substring-no-properties
                                      start end)))
                     (forward-line 1)))
                 (with-temp-buffer
                   (ert--insert-infos result)
                   (insert "    ")
                   (let ((print-escape-newlines t)
                         (print-level 5)
                         (print-length 10))
                     (ert--pp-with-indentation-and-newline
                      (ert-test-result-with-condition-condition result)))
                   (goto-char (1- (point-max)))
                   (cl-assert (looking-at "\n"))
                   (delete-char 1)
                   (message "Test %S condition:" (ert-test-name test))
                   (message "%s" (buffer-string))))
                (ert-test-aborted-with-non-local-exit
                 (message "Test %S aborted with non-local exit"
                          (ert-test-name test)))
                (ert-test-quit
                 (message "Quit during %S" (ert-test-name test)))))
            (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                   (format-string (concat "%9s  %"
                                          (prin1-to-string (length max))
                                          "s/" max "  %S")))
              (message format-string
                       (ert-string-for-test-result result
                                                   (ert-test-result-expected-p
                                                    test result))
                       (1+ (ert--stats-test-pos stats test))
                       (ert-test-name test)))))

(provide 'ert-runner-reporter-ert)
