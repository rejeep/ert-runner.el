Feature: Quiet option

  Scenario: Verbose
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (message "Funny output"))
      """
    And I run cask exec "{ERT-RUNNER} --verbose"
    Then I should see output:
      """
      Funny output
      """

  Scenario: Quiet
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (message "Funny output"))
      """
    And I run cask exec "{ERT-RUNNER} --quiet"
    Then I should not see output "Funny output"
