import java.awt._
import javax.swing._

def ticTacToeDrawer(currState: Array[Array[String]]): Unit = {
  def createLabel(text: String): JLabel = {
    new JLabel(text) {
      setFont(new Font("MV Boli", Font.PLAIN, 20))
      setVerticalTextPosition(SwingConstants.CENTER)
      setHorizontalTextPosition(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setHorizontalAlignment(SwingConstants.CENTER)
      setOpaque(true)
      setForeground(Color.black)
      setBackground(Color.blue)
      setBounds(600, 20, 300, 30)
    }
  }

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
    new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.black))
      setBounds(600, 150, 300, 300)
      for {
        row <- 0 until 3
        col <- 0 until 3
      } {
        add(createCellLabel(row, col))
      }
    }
  }

  val myFrame = new JFrame("Tic Tac Toe")

  def createMainFrame(welcomeLabel: JLabel, buttonPanel: JPanel) = {
    myFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    myFrame.setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
    val mainPanel = new JPanel(new BorderLayout()) {
      setBackground(new Color(168, 76, 162))
      add(welcomeLabel, BorderLayout.NORTH)
      add(buttonPanel, BorderLayout.CENTER)
      setLayout(null)
    }
    myFrame.add(mainPanel)
    myFrame.pack()
    myFrame.setVisible(true)
  }
  createMainFrame(createLabel("Welcome to Tic Tac Toe!"), createGamePanel())
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

