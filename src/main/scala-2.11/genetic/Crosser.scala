package genetic

import individual.Individual

/**
 * Created by Palys on 2015-10-24.
 */
trait Crosser[T <: Individual] {
  def crossOver(parent1: T, parent2: T): (T, T)
}
