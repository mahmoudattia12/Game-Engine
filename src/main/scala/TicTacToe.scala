import java.awt.*
import java.awt.event.*
import java.io.File
import javax.sound.sampled.AudioSystem
import javax.swing.*
import javax.sound.sampled.{AudioInputStream, AudioSystem, Clip}
import java.io.File
import scala.util.control.Breaks.break
def ticTacToeDrawer(currState: (Array[Array[String]], Boolean), winner: String): Unit = {
  def createCellButton(row: Int, col: Int): JButton = {
    new JButton(){
      setFont(new Font("Arial", Font.BOLD, 100))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setForeground(if (currState._1(row)(col) == "X") Color.green else Color.decode("#9000a6"))
      setText(currState._1(row)(col))
      setBorder(BorderFactory.createLineBorder(Color.yellow))
      setBackground(Color.black)
      setFocusable(false)
      setOpaque(true)
      addMouseListener(new MouseAdapter() {
        override def mouseEntered(e: MouseEvent): Unit = {
          setBackground(new Color(72, 72, 79)) // Change the background color on hover
        }
        override def mouseExited(e: MouseEvent): Unit = {
          setForeground(if (currState._1(row)(col) == "X") Color.green else Color.decode("#9000a6"))
          setBackground(Color.black)
        }
      })
      if(winner == winnerStatus.noWin) {
        addActionListener(new ActionListener() {
          def actionPerformed(e: ActionEvent): Unit = {
            if (currState._1(row)(col) == null) {
              val input: String = row.toString + " " + col.toString
              // Play audio clip
              val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/".concat(if (currState._2) "goal1.wav" else "goal2.wav")
              generateAudio(path)
              val res = ticTacToeController(currState, input)
              ticTacToeDrawer(res._1, res._2)
            }
          }
        })
      }
    }
  }

  
  def createGamePanel(): JPanel = {
    new JPanel(new GridLayout(3, 3)) {
      setBounds(550, 230, 400, 400)
      setBorder(BorderFactory.createLineBorder(Color.YELLOW, 3))
      (0 to 2).flatMap { row =>
        (0 to 2).map { col =>
          add(createCellButton(row, col))
        }
      }
    }
  }

  val colors = (Color.green, Color.decode("#9000a6"), Color.yellow)
  if(getMainFrame("Tic Tac Toe") == null) {
    createMainFrame(createLabel("Welcome to Tic Tac Toe!"), createButtonsPanelDA("Tic Tac Toe"), createGamePanel(), "Tic Tac Toe", createTurnLabel(currState._2, winner, colors))
  }
  else{
    updateFrame(createLabel("Welcome to Tic Tac Toe!"), createButtonsPanelDA("Tic Tac Toe"), createGamePanel(), "Tic Tac Toe", createTurnLabel(currState._2, winner, colors))
  }
}

def ticTacToeController(currState: (Array[Array[String]], Boolean), input: String): ((Array[Array[String]], Boolean), String) = {

  def getWinner(index: (Int, Int)): String = {
    def checkRow(row: Int, currPlay: String): Boolean = {
      LazyList.from(0).takeWhile { case (col) => col <= 2 }.foreach { case (col) =>
        if (currState._1(row)(col) != currPlay) return false
      }
      true
    }

    def checkCol(col: Int, currPlay: String): Boolean = {
      LazyList.from(0).takeWhile { case (row) => row <= 2 }.foreach { case (row) =>
        if (currState._1(row)(col) != currPlay) return false
      }
      true
    }

    val currPlay = currState._1(index._1)(index._2)
    if (checkRow(index._1, currPlay) || checkCol(index._2, currPlay)) currPlay
    else {
      index match {
        case (0, 1) | (1, 0) | (1, 2) | (2, 1) => winnerStatus.noWin
        case (0, 0) =>
          if (currState._1(1)(1) == currPlay && currState._1(2)(2) == currPlay) currPlay else winnerStatus.noWin
        case (0, 2) =>
          if (currState._1(1)(1) == currPlay && currState._1(2)(0) == currPlay) currPlay else winnerStatus.noWin
        case (2, 0) =>
          if (currState._1(1)(1) == currPlay && currState._1(0)(2) == currPlay) currPlay else winnerStatus.noWin
        case (2, 2) =>
          if (currState._1(1)(1) == currPlay && currState._1(0)(0) == currPlay) currPlay else winnerStatus.noWin
        case _ =>
          if (currState._1(0)(2) == currPlay && currState._1(2)(0) == currPlay) currPlay
          else if (currState._1(0)(0) == currPlay && currState._1(2)(2) == currPlay) currPlay
          else winnerStatus.noWin
      }
    }
  }
  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)): (Boolean, String) = {
    if (currState._1(index._1)(index._2) == null) {
      if (currState._2) currState._1(index._1)(index._2) = "X" else currState._1(index._1)(index._2) = "O"
      val winner = getWinner(index)
      if(winner == winnerStatus.noWin){
        if(isFull(3,3, currState._1)) (true, winnerStatus.full) else (true, winner)
      }else{
        (true, winner)
      }
    } else (false, winnerStatus.noWin)
  }
  val inputArr = splitString(input)
  val index: (Int, Int) = (inputArr(0).toInt, inputArr(1).toInt)
  val res = setCell(index)
  if (res._1) ((currState._1, !currState._2), res._2) else (currState, res._2)
}



