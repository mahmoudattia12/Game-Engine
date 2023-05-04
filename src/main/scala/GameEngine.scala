def gameEngine(controller: ((Array[Array[String]], Boolean), String) => (Boolean, Array[Array[String]])
               , drawer: (Array[Array[String]]) => (Unit), grid : Array[Array[String]]) = {
  var gameGrid: Array[Array[String]] = grid
  var turn1: Boolean = true;
  //draw initial grid
  drawer(gameGrid)
  while (true) {
    if (turn1) println("player 1") else println("player 2")
    scala.Predef.print("Enter your input (separated by spaces if multiple): ")
    val input = scala.io.StdIn.readLine();
    val res = controller((gameGrid, turn1), input)
    if (res._1) {
      gameGrid = res._2
      drawer(gameGrid)
      turn1 = !turn1
    } else {
      println("Not Valid input!!")
      println()
    }
  }
}
