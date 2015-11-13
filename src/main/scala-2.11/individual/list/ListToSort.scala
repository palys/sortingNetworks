package individual.list

import individual.Individual

import scala.util.Random

/**
 * Created by Palys on 2015-10-24.
 */
object ListToSort {
  val random = Random

  def randomList(size: Int) = {
    new ListToSort((1 to size).map(x => random.nextInt()).toArray)
  }
}

class ListToSort(array: Array[Int]) extends Individual {

  def this(length: Int) = this(new Array[Int](length))

  def isSorted: Boolean = {
    array.view.zip(array.tail).forall(x => x._1 <= x._2)
  }

  def set(index: Int, value: Int): ListToSort = {
    val clone = array.clone()
    clone(index) = value
    new ListToSort(clone)
  }

  def get(index: Int): Int = {
    array(index)
  }

  def getArray = array

}
