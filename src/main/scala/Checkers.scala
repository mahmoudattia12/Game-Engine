import java.awt.*
import java.awt.event.*
import javax.swing.*
import scala.annotation.tailrec

var firstInput: (Int, Int) = (-1, -1)
def checkersDrawers(currState: (Array[Array[String]], Boolean), winner: String): Unit = {
  def createCellButton(colorB: Color, colorF: Color, row: Int, col: Int): JButton = {
    new JButton() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 55))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(colorB)
      if (currState._1(row)(col) != null) setText("\uD83D\uDD34") else setText(null)
      setForeground(colorF)
      setOpaque(true)
      setFocusable(false)
      setMargin(new Insets(0, 0, 0, 0))

      addMouseListener(new MouseAdapter() {
        override def mouseEntered(e: MouseEvent): Unit = {
          if (colorB == Color.white) setBackground(new Color(200, 200, 200)) else setBackground(new Color(70, 70, 70)) // Change the background color on hover
        }

        override def mouseExited(e: MouseEvent): Unit = {
          setForeground(colorF)
          setBackground(colorB)
        }
      })
      val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/click.wav"
      if (winner == winnerStatus.noWin) {
        addActionListener(new ActionListener() {
          def actionPerformed(e: ActionEvent): Unit = {
            if (firstInput == (-1, -1)) {
              if ((currState._2 && currState._1(row)(col) == "1") || (!currState._2 && currState._1(row)(col) == "2")) {
                generateAudio(path)
                firstInput = (row, col)
              }
            }else if(firstInput != (row, col)) {
              generateAudio(path)
              val input = s"${firstInput._1} ${firstInput._2} $row $col"
              firstInput = (-1, -1)
              val res = checkersController(currState, input)
              checkersDrawers(res._1, res._2)
            }
          }
        })
      }
    }
  }

  def createGamePanel(): JPanel = {
    new JPanel(new GridLayout(8, 8)) {
      (0 until 8).flatMap { row =>
        (0 until 8).map { col =>
          val foreColor: Color = if (currState._1(row)(col) == "1") Color.red else Color.blue
          if (isSame(row, col)) {
            add(createCellButton(Color.white, foreColor, row, col))
          } else {
            add(createCellButton(Color.black, foreColor, row, col))
          }
        }
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      setBounds(500, 230, 500, 500)
    }
  }

  val colors = (Color.red, Color.blue, Color.white)
  if (getMainFrame("Checkers") == null) {
    createMainFrame(createLabel("Welcome to  Checkers!"), createButtonsPanelDA("Checkers"), createGamePanel(), "Checkers", createTurnLabel(currState._2, winner, colors))
  }
  else {
    updateFrame(createLabel("Welcome to  Checkers!"), createButtonsPanelDA("Checkers"), createGamePanel(), "Checkers", createTurnLabel(currState._2, winner, colors))
  }
}
def checkersController(currState: (Array[Array[String]], Boolean), input: String) : ((Array[Array[String]], Boolean), String) = {
  val inputArr = splitString(input)
  val from = (inputArr(0).toInt, inputArr(1).toInt)
  val to = (inputArr(2).toInt, inputArr(3).toInt)

  def validate(player: Int, oldi: Int, oldj: Int, newi: Int, newj: Int): String = {
    val validSource =
      if (player == 1)
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

  def checkForJmp(i: Int, j: Int, currentTurn: Int, isFirst: Boolean): String = {
    if (currentTurn == 1) {
      val rightChild = (i + 1, j + 1)
      val leftChild = (i + 1, j - 1)
      if ((to == rightChild || !isFirst ) && isWithin(rightChild) && currState._1(rightChild._1)(rightChild._2) == "2") {
        val rightOfRightChild = (i + 2, j + 2)
        if (isWithin(rightOfRightChild) && currState._1(rightOfRightChild._1)(rightOfRightChild._2) == null) {
          return "right jmp"
        }
      }
      if ((to == leftChild || !isFirst) && isWithin(leftChild) && currState._1(leftChild._1)(leftChild._2) == "2") {
        val leftOfLeftChild = (i + 2, j - 2)
        if (isWithin(leftOfLeftChild) && currState._1(leftOfLeftChild._1)(leftOfLeftChild._2) == null) {
          return "left jmp"
        }
      }
      null // return null as a String
    } else {
      val rightChild = (i - 1, j + 1)
      val leftChild = (i - 1, j - 1)
      if ((to == rightChild || !isFirst) && isWithin(rightChild) && currState._1(rightChild._1)(rightChild._2) == "1") {
        val rightOfRightChild = (i - 2, j + 2)
        if (isWithin(rightOfRightChild) && currState._1(rightOfRightChild._1)(rightOfRightChild._2) == null) {
          return "right jmp"
        }
      }
      if ((to == leftChild || !isFirst) && isWithin(leftChild) && currState._1(leftChild._1)(leftChild._2) == "1") {
        val leftOfLeftChild = (i - 2, j - 2)
        if (isWithin(leftOfLeftChild) && currState._1(leftOfLeftChild._1)(leftOfLeftChild._2) == null) {
          return "left jmp"
        }
      }
      null // return null as a String
    }
  }

  def jmpRecursively(i: Int, j: Int, turn: Int, isFirst: Boolean): Boolean = {
    if (!isWithin((i, j))) return false
    val check = checkForJmp(i, j, turn, isFirst)
    if (turn == 1) {
      check match {
        case "right jmp" =>
          currState._1(i)(j) = null
          currState._1(i + 1)(j + 1) = null
          currState._1(i + 2)(j + 2) = "1"
          jmpRecursively(i + 2, j + 2, turn, false)
          true
        case "left jmp" =>
          currState._1(i)(j) = null
          currState._1(i + 1)(j - 1) = null
          currState._1(i + 2)(j - 2) = "1"
          jmpRecursively(i + 2, j - 2, turn, false)
          true
        case _ =>
          false
      }
    } else {
      check match {
        case "right jmp" =>
          currState._1(i)(j) = null
          currState._1(i - 1)(j + 1) = null
          currState._1(i - 2)(j + 2) = "2"
          jmpRecursively(i - 2, j + 2, turn, false)
          true
        case "left jmp" =>
          currState._1(i)(j) = null
          currState._1(i - 1)(j - 1) = null
          currState._1(i - 2)(j - 2) = "2"
          jmpRecursively(i - 2, j - 2, turn, false)
          true
        case _ =>
          false

      }
    }
  }

  val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/eatChecker.wav"

  //get move status
  val moveStatus: String = if (currState._2) validate(1, from._1, from._2, to._1, to._2)
  else validate(2, from._1, from._2, to._1, to._2)
  if (moveStatus == "not valid") {
    (currState, winnerStatus.noWin)
  } else if (moveStatus == "check for jmp") {
    val check = if (currState._2) checkForJmp(from._1, from._2, 1, true) else checkForJmp(from._1, from._2, 2, true)
    if (check == null) (currState, winnerStatus.noWin)

    val res = if (currState._2) jmpRecursively(from._1, from._2, 1, true) else jmpRecursively(from._1, from._2, 2, true)
    if(res) {
      generateAudio(path)
      ((currState._1, !currState._2), winnerStatus.noWin)
    } else (currState, winnerStatus.noWin)
  } else {
    currState._1(from._1)(from._2) = null
    if (currState._2) currState._1(to._1)(to._2) = "1"
    else currState._1(to._1)(to._2) = "2"
    generateAudio(path)
    ((currState._1, !currState._2), winnerStatus.noWin)
  }

}

