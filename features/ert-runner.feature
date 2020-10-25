Feature: Ert Runner

  Scenario: Pattern
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      (ert-deftest foooo-test ())
      (ert-deftest bar-test ())
      (ert-deftest baz-test ())
      """
    When I run cask exec "{ERT-RUNNER} test/foo-test.el --pattern foo"
    Then I should see output:
      """
         passed  1/2  foo-test
         passed  2/2  foooo-test
      """
    And I should not see output "bar-test"
    And I should not see output "baz-test"

  Scenario: Tag filtering
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest this-test () :tags '(bar))
      (ert-deftest and-this-test () :tags '(foo))
      (ert-deftest but-not-this ())
      (ert-deftest and-not-this ())
      """
    When I run cask exec "{ERT-RUNNER} test/foo-test.el --tags foo,bar"
    Then I should see test output:
      | name          | success |
      | this-test     | t       |
      | and-this-test | t       |
      | not-this      | nil     |

  Scenario: Negative tag filtering
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest this-test () :tags '(bar))
      (ert-deftest but-not-this-test () :tags '(foo))
      (ert-deftest but-this-one ())
      (ert-deftest and-this-one-too ())
      """
    When I run cask exec "{ERT-RUNNER} test/foo-test.el --tags !foo"
    Then I should see test output:
      | name              | success |
      | this-test         | t       |
      | but-this-one      | t       |
      | and-this-one-too  | t       |
      | but-not-this-test | nil     |

  Scenario: Advanced tag filtering
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest this-test () :tags '(bar))
      (ert-deftest but-not-this-one ())
      (ert-deftest and-not-this-test () :tags '(foo))
      (ert-deftest and-not-this-one () :tags '(bar foo))
      """
    When I run cask exec "{ERT-RUNNER} test/foo-test.el --tags !foo --tags bar"
    Then I should see output:
      """
         passed  1/1  this-test
      """
    And I should not see output "not-this"

  Scenario: Advanced tag filtering
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest this-test () :tags '(bar))
      (ert-deftest but-not-this-one () :tags '(bar))
      (ert-deftest and-not-this-test () :tags '(foo))
      (ert-deftest and-not-this-one () :tags '(bar))
      """
    When I run cask exec "{ERT-RUNNER} test/foo-test.el --tags bar --pattern test"
    Then I should see output:
      """
         passed  1/1  this-test
      """
    And I should not see output "not-this"

  Scenario: Test helper
    When I create a test file called "test-helper.el" with content:
      """
      (defun foo ())
      """
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo () (foo))
      """
    When I run cask exec "{ERT-RUNNER} test/foo-test.el"
    Then I should not see error "(void-function foo)"

  Scenario: Only run specified files
    When I create a test file called "foo.el" with content:
      """
      (ert-deftest foo-test ())
      """
    When I create a test file called "bar-test.el" with content:
      """
      (ert-deftest bar-test () (error "BOOM"))
      """
    When I run cask exec "{ERT-RUNNER} test/foo.el"
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
    When I run cask exec "{ERT-RUNNER} test/foo-test.el test/bar-test.el"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: Nested test directories
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      """
    When I create a test directory called "subdir"
    When I create a test file called "subdir/bar-test.el" with content:
      """
      (ert-deftest bar-test ())
      """
    When I run cask exec "{ERT-RUNNER}"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: Nonexistent files
    When I run cask exec "{ERT-RUNNER} test/missing-test.el"
    Then I should see error "/missing-test.el` does not exist"

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
    When I run cask exec "{ERT-RUNNER}"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: Load file
    When I create a test file called "foo-init.el" with content:
      """
      (defun foo ())
      """
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (foo))
      """
    When I run cask exec "{ERT-RUNNER} --load test/foo-init.el"
    Then I should see output:
      """
         passed  1/1  foo-test
      """

  Scenario: Load file and name test file
    When I create a test file called "foo-init.el" with content:
      """
      (defun foo ())
      """
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (foo))
      """
    When I create a test file called "bar-test.el" with content:
      """
      (ert-deftest bar-test () (error "BOOM"))
      """
    When I run cask exec "{ERT-RUNNER} --load test/foo-init.el test/foo.el"
    Then I should see output:
      """
         passed  1/1  foo-test
      """
    Then I should not see error "BOOM"

  Scenario: Load path
    When I create a test file called "foo-init.el" with content:
      """
      (defun foo ())

      (provide 'foo-init)
      """
    When I create a test file called "foo-test.el" with content:
      """
      (require 'foo-init)

      (ert-deftest foo-test () (foo))
      """
    When I run cask exec "{ERT-RUNNER} -L test"
    Then I should see output:
      """
         passed  1/1  foo-test
      """
