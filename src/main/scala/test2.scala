case class Promotion(code: String, notCombinableWith: Seq[String])
case class PromotionCombo(promotionCodes: Seq[String])

object Problem2 {
  private def isCombinable(promos: Seq[Promotion]): Boolean =
    promos.forall(p => p.notCombinableWith.forall(nc => !promos.exists(_.code == nc)))

  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val allCombos = (1 to allPromotions.size).flatMap(allPromotions.combinations)
    val validCombos = allCombos.filter(isCombinable).map(p => PromotionCombo(p.map(_.code)))

    val maxByGroup = validCombos
      .groupBy(combo => combo.promotionCodes.toSet)
      .values
      .map(_.head)
      .toSeq

    maxByGroup.filterNot(c =>
      maxByGroup.exists(other =>
        other != c && c.promotionCodes.toSet.subsetOf(other.promotionCodes.toSet)
      )
    )
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    allCombinablePromotions(allPromotions).filter(_.promotionCodes.contains(promotionCode))
  }

  def main(args: Array[String]): Unit = {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )

    println("All combinations:")
    allCombinablePromotions(promotions).foreach(println)

    println("\nCombinations for P1:")
    combinablePromotions("P1", promotions).foreach(println)

    println("\nCombinations for P3:")
    combinablePromotions("P3", promotions).foreach(println)
  }
}
