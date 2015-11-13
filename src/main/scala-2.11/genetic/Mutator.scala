package genetic

import individual.Individual

/**
 * Created by Palys on 2015-10-24.
 */
trait Mutator[T <: Individual] {
  def mutate(individual: T): T
}
