package genetic.functions

import genetic.Crosser
import individual.list.ListToSort
import individual.network.{Comparator, SortingNetwork}

import scala.util.Random

/**
 * Created by Palys on 2015-11-10.
 */
object Crossers {

  object BasicListCrosser extends Crosser[ListToSort] {

    val random = Random

    override def crossOver(parent1: ListToSort, parent2: ListToSort): (ListToSort, ListToSort) = {
      val len = parent1.getArray.size
      val cutPlace = random.nextInt(len)

      val newFirst = parent1.getArray.slice(0, cutPlace) ++ parent2.getArray.slice(cutPlace, len)
      val newSecond = parent2.getArray.slice(0, cutPlace) ++ parent1.getArray.slice(cutPlace, len)

      (new ListToSort(newFirst), new ListToSort((newSecond)))
    }

  }

  object BasicNetworkCrosser extends Crosser[SortingNetwork] {
    val random = Random

    override def crossOver(parent1: SortingNetwork, parent2: SortingNetwork): (SortingNetwork, SortingNetwork) = {

      val (comps11, comps12) = randomSplit(parent1)
      val (comps21, comps22) = randomSplit(parent2)

      val newComps1 = comps11 ++ comps21
      val newComps2 = comps12 ++ comps22

      val child1 = new SortingNetwork(newComps1, parent1.listLen)
      val child2 = new SortingNetwork(newComps2, parent1.listLen)

      (child1, child2)
    }

    def randomSplit(individual: SortingNetwork): (List[Comparator], List[Comparator]) = {
      if (individual.numberOfComparators > 1) {
        val splitPlace = random.nextInt(individual.numberOfComparators - 1) + 1
        val comparators: List[Comparator] = individual.getComparators

        (comparators.take(splitPlace), comparators.takeRight(individual.numberOfComparators - splitPlace))
      } else {
        (individual.getComparators, individual.getComparators)
      }

    }
  }


}
