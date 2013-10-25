(require 'f)

(defvar ert-runner-support-path
  (f-dirname load-file-name))

(defvar ert-runner-features-path
  (f-parent ert-runner-support-path))

(defvar ert-runner-root-path
  (f-parent ert-runner-features-path))

(defvar ert-runner-project-root-path
  (f-expand "project" ert-runner-features-path))

(defvar ert-runner-project-test-path
  (f-expand "test" ert-runner-project-root-path))

(defvar ert-runner-bin-path
  (f-join ert-runner-root-path "bin" "ert-runner"))

(defvar ert-runner-output "")
(defvar ert-runner-error "")

(require 'espuds)
(require 'ansi)
(require 'ert)

(Fail
 (unless (s-blank? ert-runner-output)
   (princ "==================== ERT-RUNNER OUTPUT ====================\n")
   (princ ert-runner-output))
 (unless (s-blank? ert-runner-error)
   (princ "==================== ERT-RUNNER ERROR ====================\n")
   (princ (ansi-red "%s" ert-runner-error))))

(Before
 (mapc 'f-delete (f-glob "*.el" ert-runner-project-test-path)))
