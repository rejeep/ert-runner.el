Feature: Test Reporter

  Scenario: Default reporter
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      (ert-deftest bar-test ())
      """
    And I run cask exec "ert-runner"
    Then I should see output:
      """
         passed  1/2  bar-test
         passed  2/2  foo-test
      """

  Scenario: Dot reporter
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test ())
      (ert-deftest bar-test ())
      """
    And I run cask exec "ert-runner --reporter dot"
    Then I should see output:
      """
      ..

      Ran 2 tests in 0.
      """
