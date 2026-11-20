package rizoma.domain

class PackageLayoutSuite extends munit.FunSuite:
  test("domain packages expose validated names"):
    assertEquals(farm.Name.from("  Orchard  ").map(_.toString), Right("Orchard"))
    assertEquals(plot.Name.from("  North  ").map(_.toString), Right("North"))
    assertEquals(crop.species.CommonName.from("  Beans  ").map(_.toString), Right("Beans"))
