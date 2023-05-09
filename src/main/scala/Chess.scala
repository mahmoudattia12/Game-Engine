import java.awt.*
import java.awt.font.{FontRenderContext, TextAttribute}
import java.text.AttributedString
import javax.swing.*
import scala.annotation.tailrec
import scala.language.postfixOps

def chessDrawer(currState: Array[Array[String]]): Unit = {
  def createCellLabel(colorB: Color, colorF: Color, row: Int, col: Int): JLabel = {
  new JLabel() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 40))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(colorB)
      setForeground(colorF)
      if(currState(row)(col) != "") setText(currState(row)(col)(1).toString) else setText("")
      setOpaque(true)
    }
  }
  def createGamePanel(): JPanel = {
    val gamePanel = new JPanel(new GridLayout(8, 8)) {
      (0 until 8).flatMap { row =>
        (0 until 8).map { col =>
          val foreColor: Color = if (currState(row)(col) != "" && currState(row)(col)(0) == '2') Color.black else new Color(255, 255, 255)
          if (isSame(row, col)) {
            add(createCellLabel(new Color (234, 184, 130), foreColor, row, col))
          } else {
            add(createCellLabel(new Color(173, 96, 14), foreColor, row, col))
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
  if (getMainFrame("Chess") == null) {
    createMainFrame(createLabel("Welcome to  Chess!"), createGamePanel(), "Chess")
  }
  else {
    updateFrame(createLabel("Welcome to  Chess!"), createGamePanel(), "Chess")
  }
}
def chessController(currState: (Array[Array[String]], Boolean) , input: String): (Boolean, Array[Array[String]]) = {
  val playerTurn : Int = if(currState._2) 1 else 2
  val inputArr = splitString(input)

  //validate pieces
  def pawnCanMove(from: (Int, Int), to: (Int, Int)):Boolean = {
    val direction: Int = if(!currState._2) 1 else -1
    if(from._2 == to._2){
      if (from._1 + direction == to._1 && currState._1(to._1)(to._2) == "")  true // one move
      else if (from._1 + 2 * direction == to._1 && from._1 == (if(!currState._2) 1 else 6) &&
        currState._1(to._1)(to._2) == "" && currState._1(from._1 + direction)(to._2) == "") true   // double move
      else false
    }else if (from._1 + direction == to._1 && Math.abs(from._2 - to._2) == 1 && currState._1(to._1)(to._2) != "") true
    else false
  }
  def kingCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (Math.abs(from._1 - to._1) <= 1 && Math.abs(from._2 - to._2) <= 1) return true else false
  }
  def knightCanMove(from:(Int, Int), to:(Int,Int)): Boolean = {
    if ((Math.abs(from._1 - to._1) == 2 && Math.abs(from._2 - to._2) == 1) ||
      (Math.abs(from._1 - to._1) == 1 && Math.abs(from._2 - to._2) == 2)) return true
    else false
  }
  //used in validate queen&rock&bishop
  def checkDiagonalObstruction(fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val rowDir = if (toR > fromR) 1 else -1
    val colDir = if (toC > fromC) 1 else -1
    val (i, j) = Iterator.iterate((fromR + rowDir, fromC + colDir)) { case (r, c) => (r + rowDir, c + colDir) }
      .takeWhile { case (r, c) => r != toR && c != toC }
      .find { case (r, c) => currState._1(r)(c) != "" }
      .getOrElse((toR, toC))
    i == toR && j == toC
  }
  def checkHorizontalObstruction( fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val colDir = if (toC > fromC) 1 else -1
    val indices = LazyList.iterate(fromC + colDir)(_ + colDir).takeWhile(_ != toC)
    indices.forall(j => currState._1(toR)(j) == "")
  }
  def checkVerticalObstruction(fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val rowDir = if (toR > fromR) 1 else -1
    val i = Iterator.from(fromR + rowDir, rowDir)
    !i.takeWhile(_ != toR).exists(r => currState._1(r)(toC) != "")
  }
  def queenCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      checkDiagonalObstruction(from._1, from._2, to._1, to._2)
    }else if(from._1 == to._1){
      checkHorizontalObstruction(from._1, from._2, to._1, to._2)
    }else if(from._2 == to._2){
      checkVerticalObstruction(from._1, from._2, to._1, to._2)
    }else false
  }
  def rockCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (from._1 == to._1) {
      checkHorizontalObstruction(from._1, from._2, to._1, to._2)
    } else if (from._2 == to._2) {
      checkVerticalObstruction(from._1, from._2, to._1, to._2)
    } else false
  }
  def bishopCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      checkDiagonalObstruction(from._1, from._2, to._1, to._2)
    }else false
  }

  def canMove(from:(Int,Int), to:(Int,Int)): Boolean ={
    val piece = currState._1(from._1)(from._2)(1)
    piece match{
      case '♟' => pawnCanMove(from, to)
      case '♚' => kingCanMove(from, to)
      case '♛' => queenCanMove(from, to)
      case '♜' => rockCanMove(from, to)
      case '♝' => bishopCanMove(from, to)
      case _ => knightCanMove(from, to)
    }
  }

  if(inputArr.length == 2) {
    val from = rephrase_8x8(inputArr(0))
    val to = rephrase_8x8(inputArr(1))
    (from, to) match {
      case ((-1, _), (_, _)) => (false, currState._1)
      case ((_, -1), (_, _)) => (false, currState._1)
      case ((_, _), (-1, _)) => (false, currState._1)
      case ((_, _), (_, -1)) => (false, currState._1)
      case _ =>
        if (from._1 < 0 || from._1 >= 8 || from._2 < 0 || from._2 >= 8
          || to._1 < 0 || to._1 >= 8 || to._2 < 0 || to._2 >= 8) return (false, currState._1)

        if(currState._1(from._1)(from._2) == ""
          || currState._1(from._1)(from._2)(0) - '0' != playerTurn
          || (currState._1(to._1)(to._2) != "" &&  currState._1(to._1)(to._2)(0) - '0' == playerTurn)){
          (false, currState._1)
        }else{
          if(!canMove(from, to)) {
            (false, currState._1)
          }
          else{
            currState._1(to._1)(to._2) = currState._1(from._1)(from._2)
            currState._1(from._1)(from._2) = ""
            (true, currState._1)
          }
        }
    }
  }else (false, currState._1)
}
