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
    new SortingNetwork((1 to networkSize).map(x => new Comparator(random.nextInt(listSize), random.nextInt(listSize))).toArray)
  }
}

class SortingNetwork(comparators: Array[Comparator]) extends Individual {

  def sort(list: ListToSort): ListToSort = {

    var l = list
    comparators.foreach(c => {l = c.compare(l)})

    l
  }

  override def toString = comparators.mkString(",")

}
