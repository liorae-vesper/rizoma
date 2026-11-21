package rizoma.domain

class BusinessRegistrationSuite extends BusinessFixture:
  test("business registration generates IDs and validates references"):
    withFixture(_.seed())
