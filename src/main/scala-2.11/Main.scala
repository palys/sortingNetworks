import genetic.functions.Crossers.{BasicListCrosser, BasicNetworkCrosser}
import genetic.functions.Functions
import genetic.functions.Mutators.{BasicListMutator, BasicNetworkMutator}
import genetic.functions.Strategies.BasicStrategy
import individual.list.ListToSort
import individual.network.SortingNetwork

/**
 * Created by Palys on 2015-10-24.
 */
object Main {

  def isSorted(array: Array[Int]): Boolean = {
    array.view.zip(array.tail).forall(x => x._1 <= x._2)
  }

  def main(args: Array[String]): Unit = {
    val listLength = 5
    val listsPopulationSize = 40
    val networksPopulationSize = 40
    val startingNetworkSize = 5

    val startingLists = (1 to listsPopulationSize).map(x => ListToSort.randomList(listLength)).toArray
    val startingNetworks = (1 to networksPopulationSize).map(x => SortingNetwork.randomNetwork(listLength, startingNetworkSize)).toArray

    var populations = (startingNetworks, startingLists)

    val strategy = new BasicStrategy(listsPopulationSize, networksPopulationSize, listLength, BasicNetworkMutator,
      BasicNetworkCrosser, BasicListMutator, BasicListCrosser, Functions.networkTarget, Functions.listTarget)

    for (x <- 1 to 10) {
      populations = strategy.evolve(populations)
    }

    println("Starting lists, Starting networks")
    printFunctions(startingLists, startingNetworks)
    println("Starting lists, Final networks")
    printFunctions(startingLists, populations._1)
    println("Final lists, Starting networks")
    printFunctions(populations._2, startingNetworks)
    println("Final lists, Final networks")
    printFunctions(populations._2, populations._1)
  }

  def printFunctions(lists: Array[ListToSort], networks: Array[SortingNetwork]) = {
    println("lists:")
    println(lists.map(Functions.listTarget(_, networks)).sortBy(x => x).mkString(", "))
    println("networks:")
    println(networks.map(n => Functions.networkTarget(n, lists)).sortBy(x => x).mkString(", "))
  }
}
