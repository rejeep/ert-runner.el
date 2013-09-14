Feature: Ert Runner

  Scenario: Pattern
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      (ert-deftest foooo-test ())
      (ert-deftest bar-test ())
      (ert-deftest baz-test ())
      """
    When I run cask exec "ert-runner foo-test.el --pattern foo"
    Then I should see output:
      """
         passed  1/2  foo-test
         passed  2/2  foooo-test
      """
    And I should not see output "bar-test"
    And I should not see output "baz-test"

  Scenario: Test helper
    When I create a test file called "test-helper.el" with content:
      """
      (defun foo ())
      """
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo () (foo))
      """
    When I run cask exec "ert-runner foo-test.el"
    Then I should not see error "(void-function foo)"

  Scenario: Only run specified files
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      """
    When I create a test file called "bar-test.el" with content:
      """
      (ert-deftest bar-test () (error "BOOM"))
      """
    When I run cask exec "ert-runner foo-test.el"
    Then I should not see error "BOOM"

  Scenario: Run multiple files
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      """
    When I create a test file called "bar-test.el" with content:
      """
      (ert-deftest bar-test ())
      """
    When I run cask exec "ert-runner foo-test.el bar-test.el"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: Run all files ending with -test.el automatically
    When I create a test file called "foo.el" with content:
      """
      ;; not a test
      """
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      """
    When I create a test file called "bar-test.el" with content:
      """
      (ert-deftest bar-test ())
      """
    When I run cask exec "ert-runner"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: Load files
    When I create a test file called "foo-init.el" with content:
      """
      (defun foo ())
      """
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (foo))
      """
    When I run cask exec "ert-runner --load test/foo-init.el"
    Then I should see output:
      """
         passed  1/1  foo-test
      """
