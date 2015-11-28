import scala.util.Random

/**
Start date Feb 1, 8 am
Every hour 7 shipments
24/7 deliveries
First 15 days only < 15t
First 15 days ordered by priority ASC, weight ASC
Max 60 days
After 15 days ordered by priority ASC, weight DESC
After 15 days, all weights
*/
object Main extends App {
  import Spec._

  val r = new Random
  val all = (1 to 300).map(i => Shipment(i, r.nextInt(3) + 1, Math.floor(r.nextDouble * 50.0 * 100) / 100))

  val lowSorted = all.filter(_.w < WeightLimit)
                      .sortWith(_.w < _.w).sortWith(_.p < _.p)

  val days = (1 to TotalDays)
  val times = days.flatMap(d => (1 to HoursPerDay).map(Time(d, _))).drop(StartFirstDayAtHour - 1).iterator

  val first15 = lowSorted.grouped(DeliveriesPerHour).zip(times)
                         .take(DaysWithLowLoad * HoursPerDay)
                         .map(t => Delivery(t._1, t._2)).toList
  val usedDuringFirst15 = lowSorted.take(DeliveriesPerHour * TotalDays * HoursPerDay)

  val after15 = all.filterNot(usedDuringFirst15.contains(_))
                   .sortWith(_.w > _.w).sortWith(_.p < _.p)
                   .grouped(DeliveriesPerHour)
                   .zip(times.drop(DaysWithLowLoad * HoursPerDay 
                      - Math.floorDiv(usedDuringFirst15.size + (DeliveriesPerHour - 1), DeliveriesPerHour)))
                   .map(t => Delivery(t._1, t._2))

  println(first15.toList)
  println(after15.toList)
}

object Spec {
  val StartFirstDayAtHour = 8
  val WeightLimit = 15.0
  val TotalDays = 60
  val DeliveriesPerHour = 7
  val DaysWithLowLoad = 15
  val HoursPerDay = 24
}

case class Shipment(id: Int, p: Int, w: Double)
case class Delivery(l: IndexedSeq[Shipment], t: Time) {
  override def toString() = {
    s"Delivery[day ${t.d}, ${t.h}:00: $l]"
  }
}
case class Time(d: Int, h: Int)