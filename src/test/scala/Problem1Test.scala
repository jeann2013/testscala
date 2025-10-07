import org.scalatest.funsuite.AnyFunSuite

class Problem1Test extends AnyFunSuite {

  test("getBestGroupPrices should return lowest price per group and cabin") {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val prices = Seq(
      CabinPrice("CA", "M1", 200),
      CabinPrice("CA", "M2", 250),
      CabinPrice("CA", "S1", 225),
      CabinPrice("CA", "S2", 260),
      CabinPrice("CB", "M1", 230),
      CabinPrice("CB", "M2", 260),
      CabinPrice("CB", "S1", 245),
      CabinPrice("CB", "S2", 270)
    )

    val result = Problem1.getBestGroupPrices(rates, prices)

    val expected = Seq(
      BestGroupPrice("CA", "M1", 200, "Military"),
      BestGroupPrice("CA", "S1", 225, "Senior"),
      BestGroupPrice("CB", "M1", 230, "Military"),
      BestGroupPrice("CB", "S1", 245, "Senior")
    )

    assert(result == expected)
  }

  test("should ignore prices with rate codes not present in rate list") {
    val rates = Seq(
      Rate("R1", "Regular"),
      Rate("S1", "Senior")
    )

    val prices = Seq(
      CabinPrice("C1", "R1", 100),
      CabinPrice("C1", "X99", 50), // inválido
      CabinPrice("C1", "S1", 200)
    )

    val result = Problem1.getBestGroupPrices(rates, prices)
    assert(!result.exists(_.rateCode == "X99"))
    assert(result.size == 2)
  }

  test("should handle same price duplicates deterministically") {
    val rates = Seq(Rate("R1", "Regular"), Rate("R2", "Regular"))
    val prices = Seq(
      CabinPrice("C1", "R1", 100),
      CabinPrice("C1", "R2", 100)
    )

    val result = Problem1.getBestGroupPrices(rates, prices)
    // cualquiera de los dos es válido, pero solo debe haber uno
    assert(result.size == 1)
    assert(result.head.price == 100)
  }

  test("should return empty sequence when no matching rates exist") {
    val rates = Seq(Rate("R1", "Regular"))
    val prices = Seq(CabinPrice("C1", "X99", 300))
    val result = Problem1.getBestGroupPrices(rates, prices)
    assert(result.isEmpty)
  }

  test("should handle empty inputs gracefully") {
    assert(Problem1.getBestGroupPrices(Seq.empty, Seq.empty).isEmpty)
    assert(Problem1.getBestGroupPrices(Seq(Rate("A", "GroupA")), Seq.empty).isEmpty)
    assert(Problem1.getBestGroupPrices(Seq.empty, Seq(CabinPrice("C1", "A", 100))).isEmpty)
  }

  test("should produce deterministic sort order by cabin and group") {
    val rates = Seq(Rate("X1", "VIP"), Rate("X2", "Regular"))
    val prices = Seq(
      CabinPrice("B", "X1", 150),
      CabinPrice("A", "X2", 100)
    )

    val result = Problem1.getBestGroupPrices(rates, prices)
    // Orden esperado: por cabinCode ("A" antes que "B")
    assert(result.map(_.cabinCode) == Seq("A", "B"))
  }
}
