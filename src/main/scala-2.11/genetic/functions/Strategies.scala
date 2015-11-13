package genetic.functions

import genetic.{Crosser, Mutator, Strategy}
import individual.list.ListToSort
import individual.network.SortingNetwork

import scala.util.Random

/**
 * Created by Palys on 2015-11-10.
 */
object Strategies {

  abstract class AbstractStrategy(networkMutator: Mutator[SortingNetwork], networkCrosser: Crosser[SortingNetwork],
                      listMutator: Mutator[ListToSort], listCrosser: Crosser[ListToSort],
                      networkTargetFunction: (SortingNetwork, Array[ListToSort]) => Double,
                      listTargetFunction: (ListToSort, Array[SortingNetwork]) => Double) extends Strategy[SortingNetwork, ListToSort] {

    val random = Random

    def sortByRank(populations: (Array[SortingNetwork], Array[ListToSort])) = {

      val sortedNetworks = populations._1.sortBy(networkTargetFunction(_, populations._2))
      val sortedLists = populations._2.sortBy(listTargetFunction(_, populations._1))

      (sortedNetworks, sortedLists)
    }

    override def evolve(populations: (Array[SortingNetwork], Array[ListToSort])): (Array[SortingNetwork], Array[ListToSort]) = {

      val sortedPopulations = sortByRank(populations)



      val promotedListsPopulation = promotedLists(sortedPopulations._2)
      val mutatingListsPopulation = mutatingLists(sortedPopulations._2)
      val crossingListsPopulation = crossingLists(sortedPopulations._2)

      val mutatedLists = mutatingListsPopulation.map(listMutator.mutate(_))
      val crossedOverLists = crossingListsPopulation.map(lists => listCrosser.crossOver(lists._1, lists._2))
        .map(lists => Array(lists._1, lists._2)).reduce(_ ++ _)

      val newListsPopulation = promotedListsPopulation ++ mutatedLists ++ crossedOverLists ++ randomLists


      val promotedNetworksPopulation = promotedNetworks(sortedPopulations._1)
      val mutatingNetworksPopulation = mutatingNetworks(sortedPopulations._1)
      val crossingNetworksPopulation = crossingNetworks(sortedPopulations._1)

      val mutatedNetworks = mutatingNetworksPopulation.map(networkMutator.mutate(_))
      val crossedOverNetworks = crossingNetworksPopulation.map(networks => networkCrosser.crossOver(networks._1, networks._2))
        .map(networks => Array(networks._1, networks._2)).reduce(_ ++ _)

      val newNetworksPopulation = promotedNetworksPopulation ++ mutatedNetworks ++ crossedOverNetworks ++ randomNetworks

      (newNetworksPopulation, newListsPopulation)

      // ADD new random
    }

    def promotedLists(population: Array[ListToSort]) : Array[ListToSort]

    def promotedNetworks(population: Array[SortingNetwork]) : Array[SortingNetwork]

    def crossingLists(population: Array[ListToSort]) : Array[(ListToSort, ListToSort)]

    def crossingNetworks(population: Array[SortingNetwork]): Array[(SortingNetwork, SortingNetwork)]

    def mutatingLists(population: Array[ListToSort]) : Array[ListToSort]

    def mutatingNetworks(population: Array[SortingNetwork]) : Array[SortingNetwork]

    def randomLists : Array[ListToSort]

    def randomNetworks : Array[SortingNetwork]

  }

  class BasicStrategy(listPopulationSize: Int, networksPopulationSize: Int, listLength: Int,
                      networkMutator: Mutator[SortingNetwork], networkCrosser: Crosser[SortingNetwork],
                      listMutator: Mutator[ListToSort], listCrosser: Crosser[ListToSort],
                      networkTargetFunction: (SortingNetwork, Array[ListToSort]) => Double,
                      listTargetFunction: (ListToSort, Array[SortingNetwork]) => Double)
    extends AbstractStrategy(networkMutator, networkCrosser, listMutator,
      listCrosser, networkTargetFunction, listTargetFunction) {

    override def promotedLists(population: Array[ListToSort]): Array[ListToSort] = {
      population.slice(population.size * 3 / 4, population.size)
    }

    override def promotedNetworks(population: Array[SortingNetwork]): Array[SortingNetwork] = {
      population.slice(population.size * 3 / 4, population.size)
    }

    override def mutatingNetworks(population: Array[SortingNetwork]): Array[SortingNetwork] = ???

    override def crossingNetworks(population: Array[SortingNetwork]): Array[(SortingNetwork, SortingNetwork)] = ???

    override def mutatingLists(population: Array[ListToSort]): Array[ListToSort] = ???

    override def crossingLists(population: Array[ListToSort]): Array[(ListToSort, ListToSort)] = ???

    override def randomLists: Array[ListToSort] = {
      (1 to listPopulationSize/4).map(x => ListToSort.randomList(listLength)).toArray
    }

    override def randomNetworks: Array[SortingNetwork] = ???
  }
}
