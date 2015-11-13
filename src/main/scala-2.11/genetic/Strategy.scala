package genetic

import individual.Individual

/**
 * Created by Palys on 2015-11-10.
 */
trait Strategy[T1 <: Individual, T2 <: Individual] {

  def evolve(populations : (Array[T1], Array[T2])) : (Array[T1], Array[T2])
}
