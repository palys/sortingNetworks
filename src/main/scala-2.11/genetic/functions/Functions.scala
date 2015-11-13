package genetic.functions

import individual.list.ListToSort
import individual.network.SortingNetwork

/**
 * Created by Palys on 2015-11-10.
 */
object Functions {

  implicit def bool2int(b: Boolean) = if (b) 1 else 0

  def networkTarget(network: SortingNetwork, problems: Array[ListToSort]) = {
    problems.map(network.sort(_).isSorted).reduce(_ + _)
  }

  def listTarget(list: ListToSort, networks: Array[SortingNetwork]) = {
    networks.map(1 - _.sort(list).isSorted).reduce(_ + _)
  }
}
