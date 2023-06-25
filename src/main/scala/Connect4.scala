import javax.swing.*
import java.awt.*
import java.awt.event.*

//cell --> "1" || "2" || null
//"\uD83D\uDD34"
def connect4Drawer(currState: (Array[Array[String]], Boolean), winner: String): Unit = {
  def createCellButton(row: Int, col: Int): JButton = {
    new JButton() {
      setFont(new Font("Noto Color Emoji", Font.BOLD, 70))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setForeground(if (currState._1(row)(col) == "1") Color.red else if (currState._1(row)(col) == "2") Color.yellow else Color.white)
      setText("\uD83D\uDD34")
      setBorder(BorderFactory.createLineBorder(Color.blue))
      setBackground(Color.blue)
      setFocusable(false)
      setOpaque(true)
      addMouseListener(new MouseAdapter() {
        override def mouseEntered(e: MouseEvent): Unit = {
          setBackground(new Color(64, 167, 234)) // Change the background color on hover
        }

        override def mouseExited(e: MouseEvent): Unit = {
          setForeground(if (currState._1(row)(col) == "1") Color.red else if (currState._1(row)(col) == "2") Color.yellow else Color.white)
          setBackground(Color.blue)
        }
      })
      if (winner == "none") {
        addActionListener(new ActionListener() {
          def actionPerformed(e: ActionEvent): Unit = {
            if (currState._1(row)(col) == null) {
              val input: String = col.toString
              // Play audio clip
              val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/".concat(if (currState._2) "goal1.wav" else "goal2.wav")
              generateAudio(path)
              connect4Controller(currState, input)
            }
          }
        })
      }
    }
  }

  def createGamePanel(): JPanel = {
    new JPanel(new GridLayout(6, 7)) {
      setBounds(503, 230, 500, 450)
      (0 to 5).flatMap { i =>
        (0 to 6).map { j =>
          add(createCellButton(i, j))

        }
      }
      setBackground(Color.blue)
    }
  }
  val colors = (Color.red, Color.yellow, Color.white)
    if (getMainFrame("Connect 4") == null) {
      createMainFrame(createLabel("Welcome to Connect 4 !"), createGamePanel(),createGamePanel(), "Connect 4", true, createTurnLabel(currState._2, winner, colors))
    }
    else {
      updateFrame(createLabel("Welcome to Connect 4 !"), createGamePanel(), createGamePanel(), "Connect 4", true, createTurnLabel(currState._2, winner, colors))
    }
}
def connect4Controller(currState: (Array[Array[String]], Boolean), input: String) = {
  def getRow(col: Int): (Int) = {
    for (i <- (0 to 5)) {
      currState._1(i)(col) match {
        case "1" =>
          return i - 1
        case "2" =>
          return i - 1
        case _ =>
      }
    }
    5
  }


  def getWinner(index: (Int, Int)): String = {
    val (row, col) = index
    val currPlay = currState._1(row)(col)

    def checkVertical(r : Int, count : Int):Boolean ={
      if(count == 4) true
      else if(r+1 > 5 || currState._1(r+1)(col) != currPlay) false
      else checkVertical(r+1, count+1)
    }

    def checkHorizontalLeft(c: Int, count:Int):Int ={
      if(count == 4) count
      else if(c-1 < 0 || currState._1(row)(c-1) != currPlay) count
      else checkHorizontalLeft(c-1, count+1)
    }

    def checkHorizontalRight(c:Int, count:Int):Int={
      if (count == 4) count
      else if (c + 1 > 6 || currState._1(row)(c+1) != currPlay) count
      else checkHorizontalRight(c + 1, count + 1)
    }
    def checkHorizontal():Boolean={
      if(checkHorizontalLeft(col,1) + checkHorizontalRight(col,1) - 1 == 4) true else false
    }

    def diagonalUR(r:Int, c:Int, count:Int):Int ={
      if(count == 4) count
      else if( r-1 < 0 || c+1 > 6 || currState._1(r-1)(c+1) != currPlay) count
      else diagonalUR(r-1,c+1, count+1)
    }

    def diagonalDL(r: Int, c: Int, count: Int): Int = {
      if (count == 4) count
      else if (r + 1 > 5 || c - 1 < 0 || currState._1(r + 1)(c - 1) != currPlay) count
      else diagonalDL(r + 1, c - 1, count + 1)
    }

    def diagonalUL(r: Int, c: Int, count: Int): Int = {
      if (count == 4) count
      else if (r - 1 < 0 || c - 1 < 0 || currState._1(r - 1)(c - 1) != currPlay) count
      else diagonalUL(r - 1, c - 1, count + 1)
    }

    def diagonalDR(r: Int, c: Int, count: Int): Int = {
      if (count == 4) count
      else if (r + 1 > 5 || c + 1 > 6 || currState._1(r + 1)(c + 1) != currPlay) count
      else diagonalDR(r + 1, c + 1, count + 1)
    }

    def checkDiagonal():Boolean ={
      if(diagonalUR(row,col,1) + diagonalDL(row,col,1)-1 == 4) true
      else if(diagonalUL(row,col,1) + diagonalDR(row,col,1) - 1 == 4) true
      else false
    }
    if(checkVertical(row, 1) || checkHorizontal() || checkDiagonal()) currPlay else "none"
  }
  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)): (Boolean,String) = {
    index._1 match {
      case -1 => (false, "none")
      case _ =>
        if (currState._2) currState._1(index._1)(index._2) = "1" else currState._1(index._1)(index._2) = "2"
        val winner = getWinner(index)
        if (winner == "none") {
          if (isFull(6, 7, currState._1)) (true, "full") else (true, winner)
        } else {
          (true, winner)
        }
    }
  }

  val col = input.toInt
  val res = setCell(getRow(col), col)
  if (res._1) connect4Drawer((currState._1, !currState._2), res._2) else connect4Drawer(currState, res._2)

}