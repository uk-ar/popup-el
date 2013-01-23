Scenario: Simple
  When I create popup:
  """
  Line 1
  Line 2
  Line 3
  """
  Then The popup should be:
  """
  Line 1
  Line 2
  Line 3
  """
  Then Column number should be "0"

Scenario: Delete
  When I create popup:
  """
  Line 1
  Line 2
  Line 3
  """
  When I delete popup
  Then The popup should not be:
  """
  Line 1
  Line 2
  Line 3
  """

Scenario: Hide
  When I create popup:
  """
  Line 1
  Line 2
  Line 3
  """
  When I hide popup
  Then The popup should not be:
  """
  Line 1
  Line 2
  Line 3
  """

Scenario: Check column
  When I insert " "
  When I create popup:
  """
  Line 1
  Line 2
  Line 3
  """
  Then The popup should be:
  """
  Line 1
  Line 2
  Line 3
  """
  Then Column number should be "1"
