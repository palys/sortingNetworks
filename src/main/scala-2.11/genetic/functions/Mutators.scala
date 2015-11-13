package genetic.functions

import genetic.Mutator
import individual.list.ListToSort
import individual.network.SortingNetwork

import scala.util.Random

/**
 * Created by Palys on 2015-11-10.
 */
object Mutators {

  object BasicListMutator extends Mutator[ListToSort] {
    val random = Random

    override def mutate(individual: ListToSort): ListToSort = {

        val elementToChange = random.nextInt(individual.getArray.size)
        val newElement = random.nextInt()
        val newArray = individual.getArray.clone()
        newArray.update(elementToChange, newElement)
        new ListToSort(newArray)
    }
  }

  object BasicNetworkMutator extends Mutator[SortingNetwork] {
    override def mutate(individual: SortingNetwork): SortingNetwork = individual // TODO
  }
}
