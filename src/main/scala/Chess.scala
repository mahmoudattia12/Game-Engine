import java.awt.*
import java.awt.event.*
import java.awt.font.{FontRenderContext, TextAttribute}
import java.text.AttributedString
import javax.swing.*
import scala.annotation.tailrec
import scala.language.postfixOps

def chessDrawer(currState: (Array[Array[String]], Boolean), winner: String): Unit = {
  def createCellButton(colorB: Color, colorF: Color, row: Int, col: Int): JButton = {
    new JButton() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 40))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(colorB)
      setForeground(colorF)
      setBorder(BorderFactory.createLineBorder(colorB, 1))
      if (currState._1(row)(col) != "") setText(currState._1(row)(col)(1).toString) else setText("")
      setOpaque(true)
      setFocusable(false)
      setMargin(new Insets(0, 0, 0, 0))

      addMouseListener(new MouseAdapter() {
        override def mouseEntered(e: MouseEvent): Unit = {
          if (colorB == new Color(234, 184, 130)) {
            setBackground(Color.decode("#f5d5a2"))
            setBorder(BorderFactory.createLineBorder(Color.decode("#f5d5a2"), 1))
          } else {
            setBackground(Color.decode("#f09b13"))
            setBorder(BorderFactory.createLineBorder(Color.decode("#f09b13"), 1))
          }
        }

        override def mouseExited(e: MouseEvent): Unit = {
          setForeground(colorF)
          setBackground(colorB)
          setBorder(BorderFactory.createLineBorder(colorB, 1))
        }
      })

      val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/click.wav"
      if (winner == winnerStatus.noWin) {
        addActionListener(new ActionListener() {
          def actionPerformed(e: ActionEvent): Unit = {
            if (firstInput == (-1, -1)) {
              if ((currState._2 && currState._1(row)(col) != "" && currState._1(row)(col)(0) == '1') ||
                (!currState._2 && currState._1(row)(col) != "" && currState._1(row)(col)(0) == '2')) {
                generateAudio(path)
                firstInput = (row, col)
              }
            } else if (firstInput != (row, col)) {
              generateAudio(path)
              val input = s"${firstInput._1} ${firstInput._2} $row $col"
              firstInput = (-1, -1)
              val res = chessController(currState, input)
              chessDrawer(res._1, res._2)
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
          val foreColor: Color = if (currState._1(row)(col) != "" && currState._1(row)(col)(0) == '2') Color.black else new Color(255, 255, 255)
          if (isSame(row, col)) {
            add(createCellButton(new Color(234, 184, 130), foreColor, row, col))
          } else {
            add(createCellButton(new Color(173, 96, 14), foreColor, row, col))
          }
        }
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      setBounds(500, 230, 500, 500)

    }
  }

  val colors = (new Color(255, 255, 255), Color.black, Color.yellow)
  if (getMainFrame("Chess") == null) {
    createMainFrame(createLabel("Welcome to  Chess!"), createButtonsPanelDA("Chess"), createGamePanel(), "Chess", createTurnLabel(currState._2, winner, colors))
  }
  else {
    updateFrame(createLabel("Welcome to  Chess!"), createButtonsPanelDA("Chess"), createGamePanel(), "Chess", createTurnLabel(currState._2, winner, colors))
  }
}
def chessController(currState: (Array[Array[String]], Boolean), input: String): ((Array[Array[String]], Boolean), String) = {
  val playerTurn: Int = if (currState._2) 1 else 2
  val inputArr = splitString(input)
  val from = (inputArr(0).toInt, inputArr(1).toInt)
  val to = (inputArr(2).toInt, inputArr(3).toInt)

  //validate pieces
  def pawnCanMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    val direction: Int = if (!currState._2) 1 else -1
    if (from._2 == to._2) {
      if (from._1 + direction == to._1 && currState._1(to._1)(to._2) == "") true // one move
      else if (from._1 + 2 * direction == to._1 && from._1 == (if (!currState._2) 1 else 6) &&
        currState._1(to._1)(to._2) == "" && currState._1(from._1 + direction)(to._2) == "") true // double move
      else false
    } else if (from._1 + direction == to._1 && Math.abs(from._2 - to._2) == 1 && currState._1(to._1)(to._2) != "") true
    else false
  }

  def kingCanMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    if (Math.abs(from._1 - to._1) <= 1 && Math.abs(from._2 - to._2) <= 1) return true else false
  }

  def knightCanMove(from: (Int, Int), to: (Int, Int)): Boolean = {
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

  def checkHorizontalObstruction(fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val colDir = if (toC > fromC) 1 else -1
    val indices = LazyList.iterate(fromC + colDir)(_ + colDir).takeWhile(_ != toC)
    indices.forall(j => currState._1(toR)(j) == "")
  }

  def checkVerticalObstruction(fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val rowDir = if (toR > fromR) 1 else -1
    //checks if there is no value in the sequence generated by the
    // Iterator.from operation that satisfies the condition currState._1(r)(toC) != ""
    val i = Iterator.from(fromR + rowDir, rowDir)
    !i.takeWhile(_ != toR).exists(r => currState._1(r)(toC) != "")
  }

  def queenCanMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      checkDiagonalObstruction(from._1, from._2, to._1, to._2)
    } else if (from._1 == to._1) {
      checkHorizontalObstruction(from._1, from._2, to._1, to._2)
    } else if (from._2 == to._2) {
      checkVerticalObstruction(from._1, from._2, to._1, to._2)
    } else false
  }

  def rockCanMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    if (from._1 == to._1) {
      checkHorizontalObstruction(from._1, from._2, to._1, to._2)
    } else if (from._2 == to._2) {
      checkVerticalObstruction(from._1, from._2, to._1, to._2)
    } else false
  }

  def bishopCanMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      checkDiagonalObstruction(from._1, from._2, to._1, to._2)
    } else false
  }

  def canMove(from: (Int, Int), to: (Int, Int)): Boolean = {
    val piece = currState._1(from._1)(from._2)(1)
    piece match {
      case '♟' => pawnCanMove(from, to)
      case '♚' => kingCanMove(from, to)
      case '♛' => queenCanMove(from, to)
      case '♜' => rockCanMove(from, to)
      case '♝' => bishopCanMove(from, to)
      case _ => knightCanMove(from, to)
    }
  }


  if (currState._1(from._1)(from._2) == ""
    || currState._1(from._1)(from._2)(0) - '0' != playerTurn
    || (currState._1(to._1)(to._2) != "" && currState._1(to._1)(to._2)(0) - '0' == playerTurn)) {
    (currState, winnerStatus.noWin)
  } else {
    if (!canMove(from, to)) {
      (currState, winnerStatus.noWin)
    }
    else {
      val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/eatChecker.wav"
      generateAudio(path)
      currState._1(to._1)(to._2) = currState._1(from._1)(from._2)
      currState._1(from._1)(from._2) = ""
      ((currState._1, !currState._2), winnerStatus.noWin)
    }
  }
}
