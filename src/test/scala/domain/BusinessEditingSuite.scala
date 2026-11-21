package rizoma.domain

class BusinessEditingSuite extends BusinessEditingChecks:
  test("business edits persist validated changes"):
    withFixture { fixture =>
      fixture.seed()
      val _ = editAll(fixture)
    }
