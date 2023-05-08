import java.awt.*
import javax.swing.*
import scala.annotation.tailrec

def checkersDrawers(currState: Array[Array[String]]): Unit = {
  def createCellLabel(colorB: Color, colorF: Color, text: String): JLabel = {
    new JLabel() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 40))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(colorB)
      if(text != null) setText("\uD83D\uDD34") else setText(null)
      setForeground(colorF)
      setOpaque(true)
    }
  }
  def createGamePanel(): JPanel = {
    val gamePanel = new JPanel(new GridLayout(8, 8)) {
      (0 until 8).flatMap { row =>
        (0 until 8).map { col =>
          val foreColor: Color = if (currState(row)(col) == "1") Color.red else Color.blue
          if (isSame(row, col)) {
            add(createCellLabel(Color.white, foreColor, currState(row)(col)))
          } else {
            add(createCellLabel(Color.black, foreColor, currState(row)(col)))
          }
        }
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      setBounds(550, 100, 400, 400)
    }
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createRowPanelReference(8, 40), BorderLayout.WEST)
      add(gamePanel, BorderLayout.CENTER)
      add(createColPanelReference(9, 400, 50, Array(" ", "a ", "b", "c", "d", "e", "f", "g", "h")), BorderLayout.SOUTH)
      setBounds(530, 100, 400, 400)
    }
    collectingPanel
  }
  if (getMainFrame("Checkers") == null) {
    createMainFrame(createLabel("Welcome to  Checkers!"), createGamePanel(), "Checkers")
  }
  else {
    updateFrame(createLabel("Welcome to  Checkers!"), createGamePanel(), "Checkers")
  }
}
def checkersController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  val inputArr = splitString(input)
  def validate(player: Int, oldi: Int, oldj: Int, newi: Int, newj: Int): String = {
    val validSource =
      if(player == 1)
        currState._1(oldi)(oldj) == "1"
      else
        currState._1(oldi)(oldj) == "2"

    val moveIsValid =
      if (player == 1)
        newi - oldi == 1 && Math.abs(newj - oldj) == 1
      else
        oldi - newi == 1 && Math.abs(newj - oldj) == 1

    if (validSource && moveIsValid) {
      currState._1(newi)(newj) match {
        case null => "just move"
        case "1" =>
          if (player == 2) "check for jmp" else "not valid"
        case "2" =>
          if (player == 1) "check for jmp" else "not valid"
        case _ => "not valid"
      }
    } else {
      "not valid"
    }
  }
  def isWithin(pos: (Int, Int)): Boolean = {
    val (row, col) = pos
    row >= 0 && row < currState._1.length && col >= 0 && col < currState._1(0).length
  }
  def checkForJmp(i: Int, j: Int, currentTurn: Int): String = {
    if (currentTurn == 1) {
      val rightChild = (i + 1, j + 1)
      val leftChild = (i + 1, j - 1)
      if (isWithin(rightChild) && currState._1(rightChild._1)(rightChild._2) == "2") {
        val rightOfRightChild = (i + 2, j + 2)
        if (isWithin(rightOfRightChild) && currState._1(rightOfRightChild._1)(rightOfRightChild._2) == null) {
          return "right jmp"
        }
      }
      if (isWithin(leftChild) && currState._1(leftChild._1)(leftChild._2) == "2") {
        val leftOfLeftChild = (i + 2, j - 2)
        if (isWithin(leftOfLeftChild) && currState._1(leftOfLeftChild._1)(leftOfLeftChild._2) == null) {
          return "left jmp"
        }
      }
      null // return null as a String
    } else {
      val rightChild = (i - 1, j + 1)
      val leftChild = (i - 1, j - 1)
      if (isWithin(rightChild) && currState._1(rightChild._1)(rightChild._2) == "1") {
        val rightOfRightChild = (i - 2, j + 2)
        if (isWithin(rightOfRightChild) && currState._1(rightOfRightChild._1)(rightOfRightChild._2) == null) {
          return "right jmp"
        }
      }
      if (isWithin(leftChild) && currState._1(leftChild._1)(leftChild._2) == "1") {
        val leftOfLeftChild = (i - 2, j - 2)
        if (isWithin(leftOfLeftChild) && currState._1(leftOfLeftChild._1)(leftOfLeftChild._2) == null) {
          return "left jmp"
        }
      }
      null // return null as a String
    }
  }
  @tailrec
  def jmpRecursively(i: Int, j: Int, turn: Int): Unit = {
    if (!isWithin((i, j))) return
    val check = checkForJmp(i, j, turn)
    println("check from recur : " + check)
    if(turn == 1){
      check match {
        case "right jmp" =>
          println("right jump")
          currState._1(i)(j) = null
          currState._1(i + 1)(j + 1) = null
          currState._1(i + 2)(j + 2) = "1"
          jmpRecursively(i + 2, j + 2, turn)
        case "left jmp" =>
          println("left jump")
          currState._1(i)(j) = null
          currState._1(i + 1)(j - 1) = null
          currState._1(i + 2)(j - 2) = "1"
          jmpRecursively(i + 2, j - 2, turn)
        case _ =>
          println("ff")
      }
    }else{
      check match {
        case "right jmp" =>
          println("right jump")
          currState._1(i)(j) = null
          currState._1(i - 1)(j + 1) = null
          currState._1(i - 2)(j + 2) = "2"
          jmpRecursively(i - 2, j + 2, turn)
        case "left jmp" =>
          println("left jump")
          currState._1(i)(j) = null
          currState._1(i - 1)(j - 1) = null
          currState._1(i - 2)(j - 2) = "2"
          jmpRecursively(i - 2, j - 2, turn)
        case _ =>
          println("ff")
    }
    }
  }

  if(inputArr.length == 2){
    val from = rephrase_8x8(inputArr(0))
    val to = rephrase_8x8(inputArr(1))
    (from, to) match {
      case ((-1, _),(_,_)) => (false, currState._1)
      case ((_, -1),(_,_)) => (false, currState._1)
      case ((_, _),(-1,_)) => (false, currState._1)
      case ((_, _),(_,-1)) => (false, currState._1)
      case _ =>
        //get move status
        val moveStatus: String = if (currState._2) validate(1, from._1, from._2, to._1, to._2)
                                else validate(2, from._1, from._2, to._1, to._2)
        if(moveStatus == "not valid"){
          (false, currState._1)
        }else if(moveStatus == "check for jmp"){
          val check = if (currState._2) checkForJmp(from._1, from._2, 1) else checkForJmp(from._1, from._2, 2)
          if(check == null) return (false,currState._1)
          
          if(currState._2) jmpRecursively(from._1, from._2, 1)
          else  jmpRecursively(from._1, from._2, 2)
          
          (true, currState._1)
        }else{
          currState._1(from._1)(from._2) =  null
          if(currState._2) currState._1(to._1)(to._2) = "1"
          else currState._1(to._1)(to._2) = "2"
          (true, currState._1)
        }
    }
  }else (false,currState._1)
}

