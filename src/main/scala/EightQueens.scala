import java.awt._
import javax.swing._

def eightQueensDrawer(currState: Array[Array[String]]): Unit = {
  def createCellLabel(color: Color, row: Int, col: Int): JLabel = {
    new JLabel() {
      setFont(new Font("Arial Unicode MS", Font.BOLD, 37))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(color)
      setForeground(Color.black)
      setText(currState(row)(col))
      setOpaque(true)
    }
  }
  def createGamePanel(): JPanel = {
    val gamePanel = new JPanel(new GridLayout(8, 8)) {
      (0 until 8).flatMap { row =>
        (0 until 8).map { col =>
          isSame(row, col) match {
            case true =>
              add(createCellLabel(Color.white, row, col))
            case _ =>
              add(createCellLabel(Color.gray, row, col))
          }
        }
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
    }
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createRowPanelReference(8, 40), BorderLayout.WEST)
      add(gamePanel, BorderLayout.CENTER)
      add(createColPanelReference(9, 400, 50, Array(" ", "a ", "b", "c", "d", "e", "f", "g", "h")), BorderLayout.SOUTH)
      setBounds(530, 100, 400, 400)
    }
    collectingPanel
  }
  if (getMainFrame("8 Queens") == null) {
    createMainFrame(createLabel("Welcome to 8 Queens!"), createGamePanel(), "8 Queens")
  }
  else {
    updateFrame(createLabel("Welcome to 8 Queens!"), createGamePanel(), "8 Queens")
  }
}
def eightQueensController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  val inputArr = splitString(input)
  def ValidateUpLeft(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 - 1, -1).zip(LazyList.from(index._2 - 1, -1)).takeWhile { case (i, j) => i >= 0 && j >= 0 }.foreach { case (i, j) =>
      currState._1(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }
  def ValidateUpRight(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 - 1, -1).zip(LazyList.from(index._2 + 1)).takeWhile { case (i, j) => i >= 0 && j < 8 }.foreach { case (i, j) =>
      currState._1(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }
  def ValidateDownLeft(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 + 1).zip(LazyList.from(index._2 - 1, -1)).takeWhile { case (i, j) => i < 8 && j >= 0 }.foreach { case (i, j) =>
      currState._1(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }
  def ValidateDownRight(index: (Int, Int)): Boolean = {
    LazyList.from(index._1 + 1).zip(LazyList.from(index._2 + 1)).takeWhile { case (i, j) => i < 8 && j < 8 }.foreach { case (i, j) =>
      currState._1(i)(j) match {
        case null =>
        case _ =>
          return false
      }
    }
    true
  }
  def isValid(index: (Int, Int)): Boolean = {
    if (currState._1(index._1)(index._2) == null) {
      for (i <- 0 to 7) if (currState._1(i)(index._2) != null) return false
      for (e <- currState._1(index._1)) if (e != null) return false
      return ValidateUpRight(index) && ValidateDownRight(index) && ValidateUpLeft(index) && ValidateDownLeft(index)
    }
    false
  }
  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)): Boolean = {
    if (isValid(index)) {
      currState._1(index._1)(index._2) = "♛"; true
    } else false
  }
  def removeCell(index: (Int, Int)): Boolean = {
    if(currState._1(index._1)(index._2) == "♛"){
      currState._1(index._1)(index._2) = null; true
    }else
      false
  }
  if(inputArr(0) == "remove"){
    val cell = rephrase_8x8(inputArr(1))
    cell match {
      case (_, -1) => (false, currState._1)
      case (-1, _) => (false, currState._1)
      case _ =>
        (removeCell(cell), currState._1)
    }
  }else{
    val cell = rephrase_8x8(input)
    cell match {
      case (_, -1) => (false, currState._1)
      case (-1, _) => (false, currState._1)
      case _ =>
        (setCell(cell), currState._1)
    }
  }

}
