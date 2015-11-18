package individual.network

import individual.Individual
import individual.list.ListToSort

import scala.util.Random

/**
 * Created by Palys on 2015-10-24.
 */
object SortingNetwork {

  val random = Random

  def randomNetwork(listSize : Int, networkSize: Int) = {
    new SortingNetwork((1 to networkSize).map(x => new Comparator(random.nextInt(listSize), random.nextInt(listSize))).toList, listSize)
  }
}

class SortingNetwork(comparators: List[Comparator], listLength : Int) extends Individual {

  def sort(list: ListToSort): ListToSort = {

    var l = list
    comparators.foreach(c => {l = c.compare(l)})

    l
  }

  override def toString = comparators.mkString(",")

  def listLen = listLength

  def addComparator(c : Comparator) = {
    new SortingNetwork(comparators.map(a => a) :+ c, listLength)
  }

  def removeComparator(number: Int) = {
    new SortingNetwork(comparators.filterNot(_.equals(comparators(number))), listLength)
  }

  def numberOfComparators = comparators.length
}
