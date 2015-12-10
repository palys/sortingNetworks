import genetic.functions.Crossers.{BasicListCrosser, BasicNetworkCrosser}
import genetic.functions.{SimulationOutputter, Functions, Simulation}
import genetic.functions.Mutators.{BasicListMutator, BasicNetworkMutator}
import genetic.functions.Strategies.{EvolveOnlyNetworksStrategy, BasicStrategy}
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
    val listLength = 15
    val listsPopulationSize = 200
    val networksPopulationSize = 200
    val startingNetworkSize = 500
    val numberOfSteps = 1000

    val strategy = new BasicStrategy(listsPopulationSize, networksPopulationSize, listLength, BasicNetworkMutator,
      BasicNetworkCrosser, BasicListMutator, BasicListCrosser, Functions.lengthAwareNetworkTarget, Functions.listTarget)

    val simulation = new Simulation(listLength,listsPopulationSize,networksPopulationSize,startingNetworkSize,numberOfSteps,strategy)
    val outputer = new SimulationOutputter(simulation)

    outputer.simulateAndOutput(5)


  }


}
