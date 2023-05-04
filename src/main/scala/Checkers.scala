import java.awt._
import javax.swing._

def checkersDrawers(currState: Array[Array[String]]): Unit = {
  def createLabel(text: String): JLabel = {
    new JLabel(text) {
      setFont(new Font("MV Boli", Font.PLAIN, 20))
      setVerticalTextPosition(SwingConstants.CENTER)
      setHorizontalTextPosition(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setHorizontalAlignment(SwingConstants.CENTER)
      setOpaque(true)
      setForeground(Color.white)
      setBackground(new Color(20, 55, 143))
      setBounds(600, 20, 300, 30)
    }
  }

  def createCellLabel(colorB: Color, colorF: Color, text: String): JLabel = {
    new JLabel() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 40))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(colorB)
      setForeground(colorF)
      setText(text)
      setOpaque(true)
    }
  }

  val isSame = (x: Int, y: Int) => {
    val isEven = (a: Int) => a % 2 == 0
    (isEven(x) && isEven(y)) || (!isEven(x) && !isEven(y))
  }

  def createGamePanel(): JPanel = {
    new JPanel(new GridLayout(8, 8)) {
      for {
        row <- 0 until 8
        col <- 0 until 8
      } {
        val foreColor: Color = if (row <= 2) Color.red else if (row >= 5) Color.blue else Color.white
        if (isSame(row, col)) {
          add(createCellLabel(Color.white, foreColor, currState(row)(col)))
        } else {
          add(createCellLabel(Color.black, foreColor, currState(row)(col)))
        }
      }
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      setBounds(550, 100, 400, 400)
    }
  }

  def createMainFrame(welcomeLabel: JLabel, buttonPanel: JPanel) = {
    val mainPanel = new JPanel(new BorderLayout()) {
      setBackground(new Color(238, 162, 226))
      welcomeLabel.setBounds(600, 20, 300, 30)
      buttonPanel.setBounds(550, 100, 400, 400)
      add(welcomeLabel, BorderLayout.NORTH)
      add(buttonPanel, BorderLayout.CENTER)
      setLayout(null)
    }
    new JFrame("Checkers") {
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
      add(mainPanel)
      pack()
      setVisible(true)
    }
  }
  createMainFrame(createLabel("Welcome to  Checkers!"), createGamePanel())
}
def checkersController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  //function to rephrase input
  def getCol(c: Char): Int = {
    c match {
      case 'a' => 0
      case 'b' => 1
      case 'c' => 2
      case 'd' => 3
      case 'e' => 4
      case 'f' => 5
      case 'g' => 6
      case 'h' => 7
      case _ => -1
    }
  }

  def getRow(c: Char): Int = {
    val isWithinRange = (value: Int, min: Int, max: Int) => value >= min && value <= max
    val row: Int = 8 - (c.toInt - '0'.toInt)
    isWithinRange(row, 0, 7) match {
      case true => row
      case _ => -1
    }
  }

  val rephrase = (str: String) => (getRow(str(0)), getCol(str(1)))

  def isValid(index: (Int, Int)): Boolean = {
    true
  }

  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)): Boolean = {
    true
  }

  rephrase(input) match {
    case (_, -1) => (false, currState._1)
    case (-1, _) => (false, currState._1)
    case _ =>
      (setCell(rephrase(input)), currState._1)
  }
}

