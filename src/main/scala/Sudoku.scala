import org.jpl7.{Compound, Query}
import collection.JavaConverters.*
import javax.swing.*
import java.awt.*
import java.awt.event.*
import javax.swing.event.*
import javax.swing.text.{AttributeSet, PlainDocument}
def sudokuDrawer(currState: Array[Array[String]], winner: String): Unit = {
  def createCellField(row: Int, col: Int): JTextField = {
    val textField = new JTextField() {
      setFont(new Font("Arial", Font.BOLD, 30))
      setHorizontalAlignment(SwingConstants.CENTER)
      setBorder(BorderFactory.createLineBorder(Color.black))
      setOpaque(true)
      if (winner == winnerStatus.win || winner == winnerStatus.solved) setEditable(false)
      if (currState(row)(col) != null && currState(row)(col)(0) == '0') {
        setText(currState(row)(col)(1).toString)
        setEditable(false)
        setBackground(Color.decode("#a5a8a8"))
        setForeground(Color.black)
      } else {
        setForeground(Color.blue)
        setText(currState(row)(col))
      }
    }
    // Add a KeyListener to validate and handle user input
    textField.addKeyListener(new KeyListener {
      override def keyTyped(e: KeyEvent): Unit = {
        val c = e.getKeyChar
        val isBetweenOneAndNine = (c: Char) => c.isDigit && c >= '1' && c <= '9'
        if (!isBetweenOneAndNine(c) || c == KeyEvent.VK_BACK_SPACE || c == KeyEvent.VK_DELETE) {
          if (c != KeyEvent.VK_BACK_SPACE && c != KeyEvent.VK_DELETE)generateErrorAudio()
          currState(row)(col) = null
          e.consume()
        }
      }
      override def keyPressed(e: KeyEvent): Unit = {}
      override def keyReleased(e: KeyEvent): Unit = {
        val input = textField.getText.trim
        if (input.length > 1) {
          generateErrorAudio()
          textField.setText(input.charAt(0).toString)
        }
        if (input.length == 1) {
          val res = sudokuController(currState, s"$row $col $input")
          sudokuDrawer(res._1, res._2)
        }
      }
    })
    textField
  }

  def createBoxPanel(dx: Int, dy: Int): JPanel = {
    new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 2))
      (dx * 3 until dx * 3 + 3).flatMap { row =>
        (dy * 3 until dy * 3 + 3).map { col =>
          add(createCellField(row, col))
        }
      }
    }
  }

  def createGamePanel(): JPanel = {
    new JPanel(new GridLayout(3, 3)) {
      setBounds(500, 230, 500, 500)
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 4))
      (0 until 3).flatMap { row =>
        (0 until 3).map { col =>
          add(createBoxPanel(row, col))
        }
      }
    }
  }

  val colors = (Color.decode("#d99218"), Color.decode("#a30844"))
  if (getMainFrame("Sudoku") == null) {
    createMainFrame(createLabel("Welcome to Sudoku!"), createButtonsPanelSA("Sudoku", winner, currState), createGamePanel(), "Sudoku", createSingleAgentLabel(winner, colors))
  }
  else {
    updateFrame(createLabel("Welcome to Sudoku!"), createButtonsPanelSA("Sudoku", winner, currState), createGamePanel(), "Sudoku", createSingleAgentLabel(winner, colors))
  }
}

def sudokuController(currState: Array[Array[String]], input: String): (Array[Array[String]], String) = {
  val inputArr = splitString(input)
  //function used in prolog solver
  def makeString(currState: Array[Array[String]]): String = {
    var s: String = "Rows = ["
    for (row <- 0 until currState.length) {
      s = s + "["
      for (col <- 0 until currState(0).length) {
        currState(row)(col) match {
          case null => s += "_"
          case _ =>
            if (currState(row)(col)(0) == '0') s += currState(row)(col)(1)
            else s += currState(row)(col)
        }
        if (col != currState(0).length - 1) s += ","
        else s += "]"
      }
      if (row != currState.length - 1) s += ","
      else s += "]"
    }
    s += ", sudoku(Rows), maplist(label, Rows)."
    s
  }

  //solve the sudoku curr grid
  def prologSolve(): Boolean = {
    // Action to be performed when the button is clicked
    val consultQuery = new Query("consult('D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/src/main/scala/SudokuSolver.pl')")
    println()
    if (consultQuery.hasSolution) {
      println("Prolog file consulted successfully")
    } else {
      println("Failed to consult Prolog file")
    }
    val prologCode = makeString(currState)
    val query = new Query(prologCode)
    if (query.hasSolution) {
      val solution = query.oneSolution()
      val rowsTerm = solution.get("Rows")
      if (rowsTerm.isList()) {
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
            my2DArray(row)(col) = value
          }
        }
        for (row <- 0 until currState.length) {
          for (col <- 0 until currState(0).length) {
            if (currState(row)(col) == null) currState(row)(col) = my2DArray(row)(col).toString
          }
        }
        true
      } else {
        false
      }
    } else {
      false
    }
  }

  //x = index._1/3 * 3, y = index._2/3 *3
  def validateBlock(x: Int, y: Int, cell: String): Boolean = {
    (x until x + 3).flatMap { row =>
      (y until y + 3).map { col =>
        if ((currState(row)(col) != null && currState(row)(col).length == 2 && currState(row)(col)(1).toString == cell)
          || (currState(row)(col) == cell)) return false
      }
    }
    true
  }

  def validateRow(index: (Int, Int), cell: String): Boolean = {
    LazyList.from(0).takeWhile { case (j) => j <= 8 }.foreach { case (j) =>
      if ((currState(index._1)(j) != null && currState(index._1)(j).length == 2 && currState(index._1)(j)(1).toString == cell)
        || (currState(index._1)(j) == cell)) return false
    }
    true
  }

  def validateCol(index: (Int, Int), cell: String): Boolean = {
    LazyList.from(0).takeWhile { case (i) => i <= 8 }.foreach { case (i) =>
      if ((currState(i)(index._2) != null && currState(i)(index._2).length == 2 && currState(i)(index._2)(1).toString == cell)
        || (currState(i)(index._2) == cell)) return false
    }
    true
  }

  def isValid(index: (Int, Int), cell: String): Boolean = {
    if (currState(index._1)(index._2) == null) {
      return validateRow(index, cell) && validateCol(index, cell) && validateBlock(index._1 / 3 * 3, index._2 / 3 * 3, cell)
    }
    false
  }

  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int), cell: String): String = {
    if (isValid(index, cell)) {
      currState(index._1)(index._2) = cell;
      if (isFull(9, 9, currState)) winnerStatus.win else winnerStatus.noWin
    }else{
      generateErrorAudio()
      winnerStatus.noWin
    }
  }
  if (inputArr.length == 1 && inputArr(0) == "solve") {
      if (prologSolve()) (currState, winnerStatus.solved) else (currState, winnerStatus.noSol)
  }else {
      val cell = (inputArr(0).toInt, inputArr(1).toInt)
      (currState, setCell(cell, inputArr(2)))
  }
}

  def print2DArray(arr: Array[Array[String]]): Unit = {
    arr.foreach(row => println(row.mkString(" ")))
  }
  def generateErrorAudio() = {
    val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/sudokuError.wav"
    generateAudio(path)
  }