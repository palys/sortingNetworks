package genetic.functions

import genetic.Strategy
import individual.list.ListToSort
import individual.network.SortingNetwork

class Simulation(listLength: Int, listsPopulationSize: Int, networksPopulationSize: Int, startingNetworkSize: Int, numberOfSteps: Int, strategy: Strategy[SortingNetwork,ListToSort]) {

  def simulate = {
    val startingLists = (1 to listsPopulationSize).map(x => ListToSort.randomList(listLength)).toArray
    val startingNetworks = (1 to networksPopulationSize).map(x => SortingNetwork.randomNetwork(listLength, startingNetworkSize)).toArray

    var populations = (startingNetworks, startingLists)

    for (x <- 1 to numberOfSteps) {
      populations = strategy.evolve(populations)
    }

    (startingLists, startingNetworks, populations._1, populations._2)
  }

}
