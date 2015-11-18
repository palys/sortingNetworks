import genetic.functions.Crossers.{BasicListCrosser, BasicNetworkCrosser}
import genetic.functions.Functions
import genetic.functions.Mutators.{BasicListMutator, BasicNetworkMutator}
import genetic.functions.Strategies.{EvolveOnlyNetworksStrategy, BasicStrategy}
import genetic.functions.Simulation
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
    val numberOfSteps = 100

    val strategy = new BasicStrategy(listsPopulationSize, networksPopulationSize, listLength, BasicNetworkMutator,
      BasicNetworkCrosser, BasicListMutator, BasicListCrosser, Functions.lengthAwareNetworkTarget, Functions.listTarget)

    val (startingLists, startingNetworks, finalNetworks, finalLists) = Simulation.simulate(listLength,listsPopulationSize,networksPopulationSize,startingNetworkSize,numberOfSteps,strategy)

    println("Starting lists, Starting networks")
    printFunctions(startingLists, startingNetworks)
    println("Starting lists, Final networks")
    printFunctions(startingLists,finalNetworks)
    println("Final lists, Starting networks")
    printFunctions(finalLists, startingNetworks)
    println("Final lists, Final networks")
    printFunctions(finalLists, finalNetworks)
  }

  def printFunctions(lists: Array[ListToSort], networks: Array[SortingNetwork]) = {
    println("lists:")
    println(lists.map(Functions.listTarget(_, networks)).sortBy(x => x).mkString(", "))
    println("networks:")
    println(networks.map(n => Functions.lengthAwareNetworkTarget(n, lists)).sortBy(x => x).mkString(", "))
  }
}
