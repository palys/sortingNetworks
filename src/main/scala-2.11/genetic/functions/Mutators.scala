package genetic.functions

import genetic.Mutator
import individual.list.ListToSort
import individual.network.{Comparator, SortingNetwork}

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
    val random = Random

    override def mutate(individual: SortingNetwork): SortingNetwork = {
      val r = random.nextFloat()
      val listSize = individual.listLen

      if (r < 0.5) {
        changeRandomComparator(individual)
      } else {
        addRandomComparator(individual)
      }
    }

    def changeRandomComparator(network: SortingNetwork) : SortingNetwork = {
      val deleted = deleteRandomComparator(network)
      addRandomComparator(deleted)
    }

    def addRandomComparator(network: SortingNetwork): SortingNetwork = {
      val comp = new Comparator(random.nextInt(network.listLen), random.nextInt(network.listLen))
      network.addComparator(comp)
    }

    def deleteRandomComparator(network: SortingNetwork): SortingNetwork = {
      if (network.numberOfComparators > 0) {
        val compToDelete = random.nextInt(network.numberOfComparators)
        network.removeComparator(compToDelete)
      } else {
        network
      }
    }
  }
}
