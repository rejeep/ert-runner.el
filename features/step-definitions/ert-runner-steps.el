(When "^I create a test file called \"\\([^\"]+\\)\" with content:$"
  (lambda (file content)
    (f-write-text content 'utf-8 (f-expand file ert-runner-project-test-path))))

(When "^I create a test directory called \"\\([^\"]+\\)\"$"
  (lambda (directory)
    (f-mkdir (f-expand directory ert-runner-project-test-path))))

(When "^I run cask exec \"\\([^\"]+\\)\"$"
  (lambda (command)
    (setq command (s-replace "{ERT-RUNNER}" ert-runner-bin-path command))
    (let* ((buffer-name "*ert-runner-output*")
           (buffer
            (progn
              (when (get-buffer buffer-name)
                (kill-buffer buffer-name))
              (get-buffer-create buffer-name)))
           (default-directory (f-full ert-runner-project-root-path))
           (args (cons "exec" (s-split " " command)))
           (args
            (if (-contains? args "--reporter")
                args
              (append args (list "--reporter" "ert"))))
           (exit-code
            (apply
             'call-process
             (append (list "cask" nil buffer nil) args))))
      (with-current-buffer buffer
        (let ((content
               ;; to display consistent errors across emacs versions
               ;; we use the pre 25.1 style
               (replace-regexp-in-string
                "’" "'"
                (replace-regexp-in-string
                 "‘" "`"
                 (buffer-string)))))
          (cond ((= exit-code 0)
                 (setq ert-runner-output content))
                (t
                 (setq ert-runner-error content))))))))

(Then "^\\(?:I should see output:\\|I should see output \"\\(.+\\)\"\\)$"
  (lambda (expected)
    (should (s-contains? expected ert-runner-output))))

(Then "^\\(?:I should not see output:\\|I should not see output \"\\(.+\\)\"\\)$"
  (lambda (expected)
    (should-not (s-contains? expected ert-runner-output))))

(Then "^\\(?:I should see error:\\|I should see error \"\\(.+\\)\"\\)$"
  (lambda (expected)
    (should (s-contains? expected ert-runner-error))))

(Then "^\\(?:I should not see error:\\|I should not see error \"\\(.+\\)\"\\)$"
  (lambda (expected)
    (should-not (s-contains? expected ert-runner-error))))

(Then "^I should see test output:$"
  (lambda (table)
    (let* ((head (car table))
           (rows (cdr table))
           (name-index (-elem-index "name" head))
           (success-index (-elem-index "success" head)))
      (-each
       rows
       (lambda (row)
         (let ((name (nth name-index row))
               (success (read (nth success-index row))))
           (let* ((passed (format "passed  [0-9]+\/[0-9]+  %s" name))
                  (match (s-matches? passed ert-runner-output)))
             (if success
                 (should match)
               (should-not match)))))))))
