def sumConditional(map: Map[String, Int], str: String): Int = {
  map.foldLeft(0){(sum,element) =>
    if(element._1.contains(str)) sum + element._2
    else sum
  }
}
val map = Map("anurag" -> 24, "daniel" -> 23, "anushka" -> 30)
val str = "anu"
sumConditional(map, str)

def last(list: List[Int]): Int = {
  list.foldLeft(0){(lastElement,element) => element}
}
last(List(1, 1, 2, 3, 5, 8))