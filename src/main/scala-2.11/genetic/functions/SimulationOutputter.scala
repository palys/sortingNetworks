package genetic.functions

import individual.list.ListToSort
import individual.network.SortingNetwork

/**
 * Created by Palys on 2015-11-20.
 */
class SimulationOutputter(simulation: Simulation) {

  def simulateAndOutput(numberOfPrintedOut: Int) = {
    val (startingLists, startingNetworks, finalNetworks, finalLists) = simulation.simulate

    println("Starting lists, Starting networks")
    printFunctions(startingLists, startingNetworks, numberOfPrintedOut)
    println("Starting lists, Final networks")
    printFunctions(startingLists,finalNetworks, numberOfPrintedOut)
    println("Final lists, Starting networks")
    printFunctions(finalLists, startingNetworks, numberOfPrintedOut)
    println("Final lists, Final networks")
    printFunctions(finalLists, finalNetworks, numberOfPrintedOut)
  }

  def printFunctions(lists: Array[ListToSort], networks: Array[SortingNetwork], numberOfPrintedOut: Int) = {
    println("lists:")
    println(lists.map(Functions.listTarget(_, networks)).sortBy(x => x).takeRight(numberOfPrintedOut).mkString(", "))
    println("networks:")
    println(networks.map(n => Functions.lengthAwareNetworkTarget(n, lists)).sortBy(x => x).takeRight(numberOfPrintedOut).mkString(", "))
  }
}
