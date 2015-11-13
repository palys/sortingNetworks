/**
 * Created by Palys on 2015-10-24.
 */
object Main {

  def isSorted(array: Array[Int]): Boolean = {
    array.view.zip(array.tail).forall(x => x._1 <= x._2)
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 2, 3, 4)))
    println(isSorted(Array(1, 2, 4, 3)))
    println(isSorted(Array(4, 1, 2, 3)))
  }
}
