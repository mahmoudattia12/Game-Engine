import javax.swing._
import java.awt._

def connect4Drawer(currState: Array[Array[String]]): Unit = {
  def createGamePanel(): JPanel = {
    // Create a panel to hold the grid
    val gamePanel = new JPanel() {
      override def paintComponent(g: Graphics) = {
        super.paintComponent(g)
        (0 to 5).flatMap { i =>
          (0 to 6).map { j =>
            currState(i)(j) match {
              case null => g.setColor(Color.white)
              case "1" => g.setColor(Color.RED)
              case "2" => g.setColor(Color.yellow)
            }
            g.drawOval(j * 63 + 5, i * 63 + 8, 60, 60)
            g.fillOval(j * 63 + 5, i * 63 + 8, 60, 60)
          }
        }
      }
      setBackground(Color.blue)
    }
    // Create the main panel and add the components
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createColPanelReference(7, 450, 30, Array("a", "b", "c", "d", "e", "f", "g")), BorderLayout.NORTH)
      add(gamePanel, BorderLayout.CENTER)
      setBounds(525, 100, 450, 425)
    }
    collectingPanel
  }
  if (getMainFrame("Connect 4") == null) {
    createMainFrame(createLabel("Welcome to Connect 4 !"), createGamePanel(), "Connect 4")
  }
  else {
    updateFrame(createLabel("Welcome to Connect 4 !"), createGamePanel(), "Connect 4")
  }
}
def connect4Controller(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  //function to rephrase input
  def getCol(phrase: String): (Int) = {
    phrase match {
      case "a" => 0
      case "b" => 1
      case "c" => 2
      case "d" => 3
      case "e" => 4
      case "f" => 5
      case "g" => 6
      case _ => -1
    }
  }
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
  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int)): Boolean = {
    index._1 match {
      case -1 => false
      case _ =>
        if (currState._2) currState._1(index._1)(index._2) = "1" else currState._1(index._1)(index._2) = "2"
        true
    }
  }
  val col = getCol(input)
  col match {
    case -1 => (false, currState._1)
    case _ =>
      (setCell(getRow(col), col), currState._1)
  }
}