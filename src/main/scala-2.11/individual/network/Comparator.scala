package individual.network

import individual.list.ListToSort

/**
 * Created by Palys on 2015-10-24.
 */
class Comparator(index1: Int, index2: Int) {

  def compare(list: ListToSort): ListToSort = {
    val v1 = list.get(index1)
    val v2 = list.get(index2)
    var listToReturn = list

    if ((v1 < v2) ^ (index1 < index2)) {
      listToReturn = list.set(index1, v2).set(index2, v1)
    }

    listToReturn
  }

  override def toString = s"($index1, $index2)"
}
