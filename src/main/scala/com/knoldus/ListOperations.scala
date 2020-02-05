class ListOperations {

  def findSecondMax(list: List[Int]): Int = {
    @scala.annotation.tailrec
    def findSecondMaxRecursive(list: List[Int], max: Int): Int = {
      list match {
        case Nil => -1
        case first :: Nil => first
        case _ :: _ :: Nil => max
        case first :: rest => if (first > rest.head) findSecondMaxRecursive(rest, first) else findSecondMaxRecursive(rest, rest.head)
      }
    }

    findSecondMaxRecursive(list, list.head)
  }

  def findKthElement(list: List[Int], kthElementIndex: Int): Int = {
    @scala.annotation.tailrec
    def findKthElementRecursive(list: List[Int], index: Int): Int = {
      list match {
        case Nil => -1
        case head :: rest => if (index == kthElementIndex) head else findKthElementRecursive(rest, index + 1)
      }
    }

    findKthElementRecursive(list, 0)
  }

  def isPalindrome(list: List[Int]): Boolean = {
    @scala.annotation.tailrec
    def isPalindromeRecursive(list: List[Int], reverseList: List[Int]): List[Int] = {
      list match {
        case Nil => reverseList
        case head :: rest => isPalindromeRecursive(rest, head +: reverseList)
      }
    }

    val reverseList: List[Int] = isPalindromeRecursive(list, List[Int]())
    if (list.equals(reverseList)) true else false
  }

  def getReverse(list: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def getReverseRecursive(list: List[Int], reverseList: List[Int]): List[Int] = {
      list match {
        case Nil => reverseList
        case head :: rest => getReverseRecursive(rest, head +: reverseList)
      }
    }

    getReverseRecursive(list, List[Int]())
  }

  def firstEvenNumber(list: List[Int]): Int = {
    list match {
      case Nil => -1
      case head :: rest => if (head % 2 == 0) head else firstEvenNumber(rest)
    }
  }

  def removeDuplicates(list: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def removeDuplicatesRecursive(list: List[Int], updatedList: List[Int]): List[Int] = {
      list match {
        case Nil => updatedList
        case head :: Nil => updatedList :+ head
        case head :: rest => if (head != rest.head) {
          removeDuplicatesRecursive(rest, updatedList :+ head)
        }
        else {
          removeDuplicatesRecursive(rest, updatedList)
        }
      }
    }

    removeDuplicatesRecursive(list, List[Int]())
  }

  def addDuplicates(list: List[Int]): List[Int] = {
    @scala.annotation.tailrec
    def addDuplicatesRecursive(list: List[Int], updatedList: List[Int]): List[Int] = {
      list match {
        case Nil => updatedList
        case head :: rest => addDuplicatesRecursive(rest, updatedList :+ head :+ head)
      }
    }

    addDuplicatesRecursive(list, List[Int]())
  }

  def removeNthElement(list: List[Int], nthElement: Int): List[Int] = {
    @scala.annotation.tailrec
    def removeNthElementRecursive(list: List[Int], updatedList: List[Int]): List[Int] = {
      list match {
        case Nil => updatedList
        case head :: rest => if (head != nthElement) {
          removeNthElementRecursive(rest, updatedList :+ head)
        }
        else {
          removeNthElementRecursive(rest, updatedList)
        }
      }
    }

    removeNthElementRecursive(list, List[Int]())
  }

}

object ListOperations extends App {
  val listOps = new ListOperations()

  val list1 = List(1, 2, 3, 4, 5)
  listOps.findSecondMax(list1)

  val list2 = List(1, 2, 3, 4, 5)
  val kthElementIndex = 2
  listOps.findKthElement(list2, kthElementIndex)

  val list3 = List(1, 2, 3, 4, 5)
  listOps.isPalindrome(list3)

  val list4 = List(1, 2, 3, 4, 5)
  listOps.getReverse(list4)

  val list5 = List(1, 2, 3, 4, 5)
  listOps.firstEvenNumber(list5)

  val list6 = List(1, 1, 1, 2, 3, 3, 4, 5, 5, 3, 3)
  listOps.removeDuplicates(list6)

  val list7 = List(1, 2, 3, 4, 5)
  listOps.addDuplicates(list7)

  val list8 = List(1, 2, 3, 4, 5, 3)
  val nthElement = 3
  listOps.removeNthElement(list8, nthElement)
}
