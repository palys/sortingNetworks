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

    val strategy = new EvolveOnlyNetworksStrategy(listsPopulationSize, networksPopulationSize, listLength, BasicNetworkMutator,
      BasicNetworkCrosser, BasicListMutator, BasicListCrosser, Functions.networkTarget, Functions.listTarget)

    val (startingLists, startingNetworks, finalLists, finalNetworks) = Simulation.simulate(listLength,listsPopulationSize,networksPopulationSize,startingNetworkSize,numberOfSteps,strategy)

    println("Starting lists, Starting networks")
    printFunctions(startingLists, startingNetworks)
    println("Starting lists, Final networks")
    printFunctions(startingLists,finalLists)
    println("Final lists, Starting networks")
    printFunctions(finalNetworks, startingNetworks)
    println("Final lists, Final networks")
    printFunctions(finalNetworks, finalLists)
  }

  def printFunctions(lists: Array[ListToSort], networks: Array[SortingNetwork]) = {
    println("lists:")
    println(lists.map(Functions.listTarget(_, networks)).sortBy(x => x).mkString(", "))
    println("networks:")
    println(networks.map(n => Functions.networkTarget(n, lists)).sortBy(x => x).mkString(", "))
  }
}
