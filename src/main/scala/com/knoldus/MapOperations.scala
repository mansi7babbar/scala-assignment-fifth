class MapOperations {
  def sumConditional(map: Map[String, Int], str: String): Int = {
    map.foldLeft(0) { (sum, element) =>
      if (element._1.contains(str)) sum + element._2
      else sum
    }
  }

  def oddEven(map: Map[Int, List[String]]): Map[Int, List[String]] = {
    val map = Map(
      1 -> List("Sunil", "Laxmi"),
      2 -> List("Bhavya", "Sangeeta"),
      3 -> List("Arun", "Sushmita"),
      4 -> List("Jamwant")
    )

    map.foldLeft(Map.empty[Int, List[String]]){ (resultMap, mapElement) =>
      val resultList = mapElement._1 match {
        case mapElem if mapElem % 2 ==0 => mapElement._2.map(str => str(0)+"even")
        case _ => mapElement._2.map(str => str(0)+"odd")
      }
      resultMap + (mapElement._1 -> resultList)
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

  val map1 = Map(
    1 -> List("Sunil", "Laxmi"),
    2 -> List("Bhavya", "Sangeeta"),
    3 -> List("Arun", "Sushmita"),
    4 -> List("Jamwant")
  )
  mapOps.oddEven(map1).foreach(mapElem => println(mapElem))

  val list = List(1, 2, 3, 4, 5, 6, 7, 8)
  println(mapOps.findLastElement(list))
}
