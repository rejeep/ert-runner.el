Feature: Quiet option

  Scenario: Verbose
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (message "Funny output"))
      """
    And I run cask exec "ert-runner --verbose"
    Then I should see output:
      """
      Funny output
      """

  Scenario: Quiet
    When I create a test file called "foo-test.el" with content:
      """
      (ert-deftest foo-test () (message "Funny output"))
      """
    And I run cask exec "ert-runner --quiet"
    Then I should not see output "Funny output"
