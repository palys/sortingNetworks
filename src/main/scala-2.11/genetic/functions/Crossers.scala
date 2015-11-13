package genetic.functions

import genetic.Crosser
import individual.list.ListToSort
import individual.network.SortingNetwork

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
    override def crossOver(parent1: SortingNetwork, parent2: SortingNetwork): (SortingNetwork, SortingNetwork) = {
      (parent1, parent2)//TODO
    }
  }


}
