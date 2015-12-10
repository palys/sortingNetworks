package genetic.functions

import individual.Individual
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

    val bestList = finalLists.sortBy(Functions.listTarget(_, finalNetworks)).takeRight(1)(0)
    val bestNetwork = finalNetworks.sortBy(Functions.lengthAwareNetworkTarget(_, finalLists)).takeRight(1)(0)

    printBestList(bestList)
    printBestNetwork(bestNetwork)


    (startingLists, startingNetworks, finalNetworks, finalLists)
  }

  def printFunctions(lists: Array[ListToSort], networks: Array[SortingNetwork], numberOfPrintedOut: Int) = {
    println("lists:")
    println(lists.map(Functions.listTarget(_, networks)).sortBy(x => x).takeRight(numberOfPrintedOut).mkString(", "))
    println("networks:")
    println(networks.map(n => Functions.lengthAwareNetworkTarget(n, lists)).sortBy(x => x).takeRight(numberOfPrintedOut).mkString(", "))
  }

  def printBestList(list: ListToSort) = {
    println("Best list: " + list)
  }

  def printBestNetwork(network: SortingNetwork) = {
    val checker = SortingNetworkValidityChecker
    val score = checker.countValid(network)

    val correctSorted = score._1
    val max = score._2
    val length = network.getComparators.length

    println(s"Best network sorted $correctSorted 0-1 sequences out of $max. Network length is $length")
  }

}
