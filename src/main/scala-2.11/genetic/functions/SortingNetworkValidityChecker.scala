package genetic.functions

import individual.list.ListToSort
import individual.network.SortingNetwork

/**
 * Created by Palys on 2015-11-20.
 */
object SortingNetworkValidityChecker {

  def isValid(network: SortingNetwork) = {
    val len = network.listLen

    val sequencesIterator = new ZeroOneSequence(len)

    sequencesIterator.map(new ListToSort(_)).map(network.sort(_)).map(_.isSorted).reduce(_ && _)//TODO change to fail fast

  }


  class ZeroOneSequence(length: Int) extends Iterator[Array[Int]] {
    val combinations = ((1 to length).map(x => 0) ++ (1 to length).map(x => 1)).combinations(length)
    val permutations = combinations.map(_.permutations)
    var currentPermutations = permutations.next()

    override def hasNext: Boolean = {
      if (currentPermutations.hasNext) {
        true
      } else {
        permutations.hasNext
      }
    }

    override def next(): Array[Int] = {
      if (!currentPermutations.hasNext) {
        currentPermutations = permutations.next()
      }

      currentPermutations.next().toArray
    }
  }

}
