import org.scalatest.PrivateMethodTester.{PrivateMethod, anyRefToInvoker}
import org.scalatest.funsuite.AnyFunSuite

class Problem2Test extends AnyFunSuite {

  test("allCombinablePromotions should return expected combinations") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val result = Problem2.allCombinablePromotions(promotions)

    val expected = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5")),
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    assert(result.toSet == expected.toSet)
  }

  test("combinablePromotions should return only combos containing specific code") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    val resultForP1 = Problem2.combinablePromotions("P1", promotions)
    val expectedForP1 = Seq(
      PromotionCombo(Seq("P1", "P2")),
      PromotionCombo(Seq("P1", "P4", "P5"))
    )

    assert(resultForP1.toSet == expectedForP1.toSet)

    val resultForP3 = Problem2.combinablePromotions("P3", promotions)
    val expectedForP3 = Seq(
      PromotionCombo(Seq("P2", "P3")),
      PromotionCombo(Seq("P3", "P4", "P5"))
    )

    assert(resultForP3.toSet == expectedForP3.toSet)
  }

  test("isCombinable should return true for non-conflicting promotions") {
    val promos = Seq(
      Promotion("P1", Seq("P2")),
      Promotion("P3", Seq("P4"))
    )
    val method = PrivateMethod[Boolean](Symbol("isCombinable"))
    val result = Problem2 invokePrivate method(promos)
    assert(result) // ninguna se excluye entre sí
  }

  test("isCombinable should return false for conflicting promotions") {
    val promos = Seq(
      Promotion("P1", Seq("P2")),
      Promotion("P2", Seq())
    )
    val method = PrivateMethod[Boolean](Symbol("isCombinable"))
    val result = Problem2 invokePrivate method(promos)
    assert(!result)
  }

  test("allCombinablePromotions should handle empty input gracefully") {
    val result = Problem2.allCombinablePromotions(Seq.empty)
    assert(result.isEmpty)
  }
}
