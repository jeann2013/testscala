case class Rate(rateCode: String, rateGroup: String)
case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)
case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

object Problem1 {

  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {
    val rateMap = rates.map(r => r.rateCode -> r.rateGroup).toMap

    prices
      .filter(p => rateMap.contains(p.rateCode))
      .groupBy(p => (p.cabinCode, rateMap(p.rateCode)))
      .values
      .map { cabinPrices =>
        val best = cabinPrices.minBy(_.price)
        BestGroupPrice(best.cabinCode, best.rateCode, best.price, rateMap(best.rateCode))
      }
      .toSeq
      .sortBy(p => (p.cabinCode, p.rateGroup))
  }

  def main(args: Array[String]): Unit = {
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

    getBestGroupPrices(rates, prices).foreach(println)
  }
}