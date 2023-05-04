import java.awt._
import javax.swing._

def eightQueensDrawer(currState: Array[Array[String]]): Unit = {
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
        isSame(row, col) match {
          case true =>
            add(createCellLabel(Color.white, row, col))
          case _ =>
            add(createCellLabel(Color.gray, row, col))
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
    new JFrame("8 Queens") {
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
      add(mainPanel)
      pack()
      setVisible(true)
    }
  }

  createMainFrame(createLabel("Welcome to 8 Queens!"), createGamePanel())
}
def eightQueensController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
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
    c match {
      case '1' => 7
      case '2' => 6
      case '3' => 5
      case '4' => 4
      case '5' => 3
      case '6' => 2
      case '7' => 1
      case '8' => 0
      case _ => -1
    }
  }

  val rephrase = (str: String) => (getRow(str(0)), getCol(str(1)))

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

  rephrase(input) match {
    case (_, -1) => (false, currState._1)
    case (-1, _) => (false, currState._1)
    case _ =>
      (setCell(rephrase(input)), currState._1)
  }
}
