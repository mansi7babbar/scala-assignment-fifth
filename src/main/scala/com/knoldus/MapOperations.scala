class MapOperations {
  def sumConditional(map: Map[String, Int], str: String): Int = {
    map.foldLeft(0) { (sum, element) =>
      if (element._1.contains(str)) sum + element._2
      else sum
    }
  }

  def findLastElement(list: List[Int]): Int = {
    list.foldLeft(0) { (_, element) => element }
  }
}

object MapOperations extends App {
  val mapOps = new MapOperations()

  val map = Map("anurag" -> 24, "daniel" -> 23, "anushka" -> 30)
  val str = "anu"
  println(mapOps.sumConditional(map, str))

  val list = List(1, 2, 3, 4, 5, 6, 7, 8)
  println(mapOps.findLastElement(list))
}

