Feature: Test Reporter

  Scenario: ERT reporter success
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      (ert-deftest bar-test ())
      """
    And I run cask exec "{ERT-RUNNER} --reporter ert"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: ERT reporter fail
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (should nil))
      """
    And I run cask exec "{ERT-RUNNER} --reporter ert"
    Then I should see error:
      """
      Test foo-test backtrace:
      """
    And I should see error:
      """
      Test foo-test condition:

          (ert-test-failed
           ((should nil)
            :form nil :value nil))
      """
    And I should see error:
      """
         FAILED  1/1  foo-test
      """
    And I should see error:
      """
      Ran 1 tests, 0 results as expected, 1 unexpected
      """
    And I should see error:
      """
      1 unexpected results:
         FAILED  foo-test
      """

  Scenario: Dot reporter
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      (ert-deftest bar-test ())
      """
    And I run cask exec "{ERT-RUNNER} --reporter dot"
    Then I should see output:
      """
      ..

      Ran 2 tests in 0.
      """

  Scenario: Dot reporter fail
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (should nil))
      """
    And I run cask exec "{ERT-RUNNER} --reporter dot"
    Then I should see error:
      """
      F

      Ran 1 test in 0
      """
    And I should see error:
      """
      1 unexpected results:
         FAILED  foo-test
      """
    And I should see error:
      """
      Test foo-test backtrace:
      """
    And I should see error:
      """
      Test foo-test condition:

          (ert-test-failed
           ((should nil)
            :form nil :value nil))
      """
    And I should see error:
      """
      1 unexpected results:
         FAILED  foo-test
      """
