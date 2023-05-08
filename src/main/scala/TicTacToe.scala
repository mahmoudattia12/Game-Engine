import java.awt._
import javax.swing._

def ticTacToeDrawer(currState: Array[Array[String]]): Unit = {
  def createCellLabel(row: Int, col: Int): JLabel = {
    new JLabel() {
      setFont(new Font("Arial", Font.BOLD, 80))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setForeground(if (currState(row)(col) == "X") Color.green else new Color(195, 47, 188))
      setText(currState(row)(col))
      setBorder(BorderFactory.createLineBorder(Color.yellow))
      setBackground(Color.black)
      setOpaque(true)
    }
  }
  def createGamePanel(): JPanel = {
    val gamePanel = new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.YELLOW, 3))
      (0 to 2).flatMap { row =>
        (0 to 2).map { col =>
          add(createCellLabel(row, col))
        }
      }
    }
    // Create the main panel and add the components
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createRowPanelReference(3, 100), BorderLayout.WEST)
      add(gamePanel, BorderLayout.CENTER)
      add(createColPanelReference(4, 300, 100, Array(" ", "a", "b", "c")), BorderLayout.SOUTH)
      setBounds(500, 150, 400, 400)
    }
    collectingPanel
  }
  if(getMainFrame("Tic Tac Toe") == null) {
    createMainFrame(createLabel("Welcome to Tic Tac Toe!"), createGamePanel(), "Tic Tac Toe")
  }
  else{
    updateFrame(createLabel("Welcome to Tic Tac Toe!"), createGamePanel(), "Tic Tac Toe")
  }
}

def ticTacToeController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  //function to rephrase input
  def rephrase(phrase: String): (Int, Int) = {
    phrase match {
      case "1a" => (2, 0)
      case "1b" => (2, 1)
      case "1c" => (2, 2)
      case "2a" => (1, 0)
      case "2b" => (1, 1)
      case "2c" => (1, 2)
      case "3a" => (0, 0)
      case "3b" => (0, 1)
      case "3c" => (0, 2)
      case _ => (-1, -1)
    }
  }

  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)): Boolean = {
    if (currState._1(index._1)(index._2) == null) {
      if (currState._2) currState._1(index._1)(index._2) = "X"
      else currState._1(index._1)(index._2) = "O"
      true
    } else false
  }

  rephrase(input) match {
    case (-1, -1) => (false, currState._1)
    case _ =>
      (setCell(rephrase(input)), currState._1)
  }
}



