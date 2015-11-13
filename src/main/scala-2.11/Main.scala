/**
 * Created by Palys on 2015-10-24.
 */
object Main {

  def isSorted(array: Array[Int]): Boolean = {
    array.view.zip(array.tail).forall(x => x._1 <= x._2)
  }

  def main(args: Array[String]): Unit = {
    println(4 to 8 by 2)
  }
}
