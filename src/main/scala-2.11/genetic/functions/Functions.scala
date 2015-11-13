package genetic.functions

import individual.list.ListToSort
import individual.network.SortingNetwork

/**
 * Created by Palys on 2015-11-10.
 */
object Functions {

  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  def networkTarget(network: SortingNetwork, problems: Array[ListToSort]) = {
    problems.map(network.sort(_).isSorted).map(bool2int(_)).reduce(_ + _)
    // TODO add penality for size
  }

  def listTarget(list: ListToSort, networks: Array[SortingNetwork]) = {
    networks.map(_.sort(list).isSorted).map(1 - bool2int(_)).reduce(_ + _)
  }
}
