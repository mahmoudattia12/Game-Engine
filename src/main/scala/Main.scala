import org.jpl7._
import collection.JavaConverters._


object Main {

  def main(args: Array[String]): Unit = {
    //initialScreen()


    //D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/src/main/scalaSudokuSolver.pl
    //D:\CSED\level2\2nd semester\programming paradigms\project\Phase-1\Game-Engine\src\main\scala\SudokuSolver.pl
    val consultQuery = new Query("consult('D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/src/main/scala/SudokuSolver.pl')")
    if (consultQuery.hasSolution) {
      println("Prolog file consulted successfully")
    } else {
      println("Failed to consult Prolog file")
    }
    val prologCode = "Rows = [[4,9,6,7,_,1,_,_,5],[5,_,_,4,_,9,_,7,6],[7,_,_,5,_,6,9,_,_],[9,1,7,_,6,3,5,_,4],[2,4,3,_,9,5,_,6,7],[6,5,8,_,4,7,_,9,_],[8,6,5,3,1,2,7,4,9],[_,7,4,9,5,8,6,_,2],[_,2,9,6,7,4,_,5,8]], sudoku(Rows), maplist(label, Rows)."
    val query = new Query(prologCode)
    if (query.hasSolution) {
      val solution = query.oneSolution()
      val rowsTerm = solution.get("Rows")


      if (rowsTerm.isList()) {
        println("list")
        println(rowsTerm)
        // Some org.jpl7.Term object representing a list of lists
        val outerList = rowsTerm.asInstanceOf[Compound].toTermArray

        // Get the number of rows and columns
        val rows = outerList.length
        val cols = outerList.head.asInstanceOf[Compound].toTermArray.length

        // Create a 2D array
        val my2DArray = Array.ofDim[Int](rows, cols)

        // Iterate over the outer list and extract inner lists
        for (row <- 0 until rows) {
          val innerList = outerList(row).asInstanceOf[Compound].toTermArray

          // Iterate over the inner list and extract values
          for (col <- 0 until cols) {
            val value = innerList(col).intValue() // Adjust the conversion based on the Term's content type

            // Assign the value to the corresponding position in the 2D array
//            println("Value is : " + value);
            my2DArray(row)(col) = value
          }
        }
        println("Array:")
        for (row <- my2DArray) {
          for (element <- row) {
            print(s"$element ")
          }
          println() // Move to the next line after printing each row
        }
      } else {
        println("Invalid solution format")
      }
    } else {
      println("No solution found.")
    }
  }
}