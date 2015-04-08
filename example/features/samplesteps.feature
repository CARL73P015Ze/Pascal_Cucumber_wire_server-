@billing
Feature: Division
  In order to avoid silly mistakes
  Cashiers must be able to calculate a fraction

  Scenario: Regular numbers
    Given I have entered 3 into the calculator
    And I have entered 2 into the calculator
    When I press divide
    Then the result should be 1.5 on the screen

  Scenario Outline: Regular numbers again
    Given I have entered <number> into the calculator
    And I have entered <number1> into the calculator
    When I press divide
    Then the result should be <result> on the screen
  Examples:
	|number|number1|result|
	|4|2|2|
	|8|4|5|
	
  @Important
  Scenario: Converting Cells
    Given I am logged in as a buyer
    When I view warranty options
    Then I should see the following options:
	  | name	 | price |
	  | Platinum | $1000 |
	  | Gold     | $500  |
	  | Silver   | $200  |

	Scenario: Sending A Call To OnCall
		Given I have the company configuration:
			|CanSendToServer | Y            |
			|ServerHostName  | hud01         |
		When I Click Send OnCall