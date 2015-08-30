
import scala.io.StdIn

object PizzaService {

  def main(args: Array[String]) = {

    val n: Int = StdIn.readLine("Number of customers: ").toInt

    val input: Seq[String] = 1 to n map {
      i => StdIn.readLine(s"Order time of customer and cooking time for pizza #$i: ")
    }

    val orders = input.map {
      in =>
        in.split(" ") match {
          case Array(x: String, y: String) => (x.toInt, y.toInt)
          case _ => throw new Exception("Invalid input")
        }
    }

    val validPermutations = orders.permutations.flatMap {
      permutation =>
        if (permutationIsValid(permutation)) Some(permutation) else None
    }

    val avgWaitingTimes = validPermutations.map(avgWaitingTime).toSeq

    avgWaitingTimes.sorted.headOption match {
      case Some(avg) => println(s"Minimal average waiting time is: $avg")
      case None =>  println("Empty list, no averages found")
    }

  }

  def permutationIsValid(p: Seq[(Int, Int)]): Boolean = {
    p.forall {
      o =>
        val (prev, _) = p.splitAt(p.indexOf(o))
        prev.isEmpty || prev.exists {
          p =>
            o._1 >= p._1 && o._1 <= p._2
        }
    }
  }

  def avgWaitingTime(orders: Seq[(Int, Int)]): Int = {
    val s =orders.map {
      o =>
        val (prev, _) = orders.splitAt(orders.indexOf(o))
        // wt(i) = ct(0 to i) - ot(i)
        prev.map(_._2).sum + o._2 - o._1
    }
    s.sum / orders.size
  }

}
