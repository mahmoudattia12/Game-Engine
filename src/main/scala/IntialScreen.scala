import scala.util.control.Breaks.{break, breakable}

def initialCheckersGrid(): Array[Array[String]] = {
  val isSame = (x: Int, y: Int) => {
    val isEven = (a: Int) => a % 2 == 0
    (isEven(x) && isEven(y)) || (!isEven(x) && !isEven(y))
  }
  //🔴//🔵//🔴
  val temp: Array[Array[String]] = Array.ofDim[String](8, 8)
  (0 to 2).flatMap { row =>
    (0 to 7).map { col =>
      if (!isSame(row, col)) temp(row)(col) = "1"
    }
  }
  (5 to 7).flatMap { row =>
    (0 to 7).map { col =>
      if (!isSame(row, col)) temp(row)(col) = "2"
    }
  }
  return temp
}

def start() = {
  val isBetweenOneAndSix = (str: String) => str.length == 1 && str.charAt(0).isDigit && str.charAt(0) >= '1' && str.charAt(0) <= '6'
  println("\t\t\t\t\t\t\t\t\t\t\tWelcome to Our Game Engine\n")
  println("1- Checkers\t\t2- Chess\t\t3- Tic-Tac-Toe\t\t4- Connect 4\t\t5- Sudoku\t\t6- Eight Queens")
  var gameChoice: String = "1"
  breakable {
    while (true) {
      scala.Predef.print("please enter your choice(1/2/3/4/5/6): ")
      gameChoice = scala.io.StdIn.readLine()
      if (isBetweenOneAndSix(gameChoice)) break
      else {
        println("invalid input!!!")
      }
    }
  }
  gameChoice match {
    case "1" =>
      gameEngine(checkersController, checkersDrawers, initialCheckersGrid())
    case "2" =>
      val grid: Array[Array[String]] = 
        Array(Array("♜", "♞", "♝", "♛", "♚", "♝", "♞", "♜"),
        Array("♟", "♟", "♟", "♟", "♟", "♟", "♟", "♟"),
        Array("", "", "", "", "", "", "", ""),
        Array("", "", "", "", "", "", "", ""),
        Array("", "", "", "", "", "", "", ""),
        Array("", "", "", "", "", "", "", ""),
        Array("♟", "♟", "♟", "♟", "♟", "♟", "♟", "♟"),
        Array("♜", "♞", "♝", "♛", "♚", "♝", "♞", "♜"))
      gameEngine(chessController, chessDrawer, grid)
    case "3" =>
      gameEngine(ticTacToeController, ticTacToeDrawer, Array.ofDim[String](3, 3))
    case "4" =>
      gameEngine(connect4Controller, connect4Drawer, Array.ofDim[String](6, 7))
    case "5" =>
      gameEngine(sudokuController, sudokuDrawer, generateInitialSudoku())
    case _ =>
      gameEngine(eightQueensController, eightQueensDrawer, Array.ofDim[String](8, 8))

  }
}
