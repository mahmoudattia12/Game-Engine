import com.sun.java.accessibility.util.AWTEventMonitor.addActionListener
import org.jpl7.*

import java.awt.*
import java.awt.event.*
import java.io.File
import javax.swing.*
import scala.util.control.Breaks.{break, breakable}


//check if remove is clicked
var isRemove = false
def eightQueensDrawer(currState: Array[Array[String]], winner: String): Unit = {
  def createCellButton(color: Color, row: Int, col: Int): JButton = {
    new JButton() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 55))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(color)
      setForeground(Color.black)
      setFocusable(false)
      setOpaque(true)
      setText(currState(row)(col))
      setMargin(new Insets(0, 0, 0, 0))

      addMouseListener(new MouseAdapter() {
        override def mouseEntered(e: MouseEvent): Unit = {
          if (color == Color.white) setBackground(new Color(240, 240, 240)) else setBackground(new Color(110, 110, 110)) // Change the background color on hover
        }

        override def mouseExited(e: MouseEvent): Unit = {
          setForeground(Color.black)
          setBackground(color)
        }
      })
      if (winner == winnerStatus.noWin || winner == winnerStatus.noSol) {
        addActionListener(new ActionListener() {
          def actionPerformed(e: ActionEvent): Unit = {

            val input: String = row.toString + " " + col.toString
            if (currState(row)(col) == null) {
              if (!isRemove) {
                val res = eightQueensController(currState, input)
                eightQueensDrawer(res._1, res._2)
              }
            } else {
              if (isRemove) {
                val res = eightQueensController(currState, "remove " + input)
                eightQueensDrawer(res._1, res._2)
              }
            }
            isRemove = false
          }
        })
      }
    }
  }

  def createGamePanel(): JPanel = {
    new JPanel(new GridLayout(8, 8)) {
      (0 until 8).flatMap { row =>
        (0 until 8).map { col =>
          isSame(row, col) match {
            case true =>
              add(createCellButton(Color.white, row, col))
            case _ =>
              add(createCellButton(Color.gray, row, col))
          }
        }
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      setBounds(500, 230, 500, 500)
    }
  }

//  val colors = (Color.green, new Color(38, 178, 218), Color.yellow)
  val colors = (Color.decode("#d99218"), Color.decode("#a30844"))
  //edit new turnLabel for single agent
  if (getMainFrame("8 Queens") == null) {
    createMainFrame(createLabel("Welcome to 8 Queens!"), createButtonsPanelSA("8 Queens", winner, currState), createGamePanel(), "8 Queens", createSingleAgentLabel(winner, colors))
  }
  else {
    updateFrame(createLabel("Welcome to 8 Queens!"), createButtonsPanelSA("8 Queens", winner, currState), createGamePanel(), "8 Queens", createSingleAgentLabel(winner, colors))
  }
}
def eightQueensController(currState: Array[Array[String]], input: String):(Array[Array[String]],String) = {
  val inputArr = splitString(input)

  def makeString(state: Array[Array[String]]): String = {
    var s: String = "["
    for (col <- 0 until 8) {
      var flag: Boolean = false
      breakable {
        for (row <- 0 until 8) {
          if (state(row)(col) == "♛") {
            s += (8 - row).toString
            flag = true
            break
          }
        }
      }
      if (!flag) s += "_"
      if (col != 7) s += "," else s += "]"
    }
    s
  }

  def prologSolve(): Boolean = {
    val consultQuery = new Query("consult('D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/src/main/scala/8Queens.pl')")
    if (consultQuery.hasSolution) {
      println("Prolog file consulted successfully")
    } else {
      println("Failed to consult Prolog file")
    }
    val s: String = makeString(currState)
    val prologCode = "Qs = " + s + " ,eight_queens(Qs), maplist(between(1,8), Qs)."
    val query = new Query(prologCode)
    if (query.hasSolution) {
      val solution = query.oneSolution()
      val rowsTerm = solution.get("Qs")
      if (rowsTerm.isList()) {
        // Some org.jpl7.Term object representing a list of lists
        val outerList = rowsTerm.asInstanceOf[Compound].toTermArray
        // Get the number of rows and columns
        val rows = outerList.length
        // Create a 2D array
        val myArray = Array.ofDim[Int](rows)
        // Iterate over the outer list and extract inner lists
        for (row <- 0 until rows) {
          val value = outerList(row).intValue() // Adjust the conversion based on the Term's content type
          myArray(row) = value
        }
        for (col <- 0 until myArray.length) {
          currState(8 - myArray(col))(col) = "♛"
        }
        true

      } else {
        false
      }
    } else {
      false
    }
  }

  def ValidateUpLeft(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 - 1, -1).zip(LazyList.from(index._2 - 1, -1)).takeWhile { case (i, j) => i >= 0 && j >= 0 }.foreach { case (i, j) =>
      currState(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }

  def ValidateUpRight(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 - 1, -1).zip(LazyList.from(index._2 + 1)).takeWhile { case (i, j) => i >= 0 && j < 8 }.foreach { case (i, j) =>
      currState(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }

  def ValidateDownLeft(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 + 1).zip(LazyList.from(index._2 - 1, -1)).takeWhile { case (i, j) => i < 8 && j >= 0 }.foreach { case (i, j) =>
      currState(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }

  def ValidateDownRight(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 + 1).zip(LazyList.from(index._2 + 1)).takeWhile { case (i, j) => i < 8 && j < 8 }.foreach { case (i, j) =>
      currState(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }

  def isValid(index: (Int, Int)): Boolean = {
    if (currState(index._1)(index._2) == null) {
      for (i <- 0 to 7) if (currState(i)(index._2) != null) return false
      for (e <- currState(index._1)) if (e != null) return false
      return ValidateUpRight(index) && ValidateDownRight(index) && ValidateUpLeft(index) && ValidateDownLeft(index)
    }
    false
  }

  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)):String = {
    if (isValid(index)) {
      // Play audio clip
      val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/goal2.wav"
      generateAudio(path)
      currState(index._1)(index._2) = "♛";
      if(isWinner()) winnerStatus.win else winnerStatus.noWin
    } else {
      val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/error.wav"
      generateAudio(path)
      winnerStatus.noWin
    }
  }

  def removeCell(index: (Int, Int)) = {
    if (currState(index._1)(index._2) == "♛") {
      val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/remove.wav"
      generateAudio(path)
      currState(index._1)(index._2) = null;
    }
  }
  def isWinner():Boolean = {
    (0 until 8).flatMap { row =>
      var queens = 0
      (0 until 8).map { col =>
        if(currState(row)(col) == "♛") queens += 1
        if(queens > 1) return false
        if(col == 7 && queens != 1) return false
      }
    }
    true
  }
  if (inputArr(0) == "remove") {
    val cell = (inputArr(1).toInt, inputArr(2).toInt)
    removeCell(cell)
    (currState, winnerStatus.noWin)
  } else if (inputArr(0) == "solve") {
    if (prologSolve()) (currState, winnerStatus.solved)
    else (currState, winnerStatus.noSol)
  } else {
    val cell = (inputArr(0).toInt, inputArr(1).toInt)
    (currState, setCell(cell))
  }
}
