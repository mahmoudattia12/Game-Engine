import javax.swing._
import java.awt._

def connect4Drawer(currState: Array[Array[String]]): Unit = {
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

  def createColPanelReference(): JPanel = {
    val colPanel = new JPanel(new GridLayout(1, 7)) {
      setPreferredSize(new Dimension(450, 30))
      setBackground(new Color(168, 76, 162))
    }
    for (col <- Array("a", "b", "c", "d", "e", "f", "g")) {
      val label = createLabel(col)
      label.setBackground(new Color(168, 76, 162))
      label.setForeground(new Color(239, 196, 67))
      label.setVerticalAlignment(SwingConstants.CENTER)
      label.setHorizontalAlignment(SwingConstants.CENTER)
      colPanel.add(label)
    }
    colPanel
  }

  def createGamePanel(): JPanel = {
    // Create a panel to hold the grid
    val gamePanel = new JPanel() {
      override def paintComponent(g: Graphics) = {
        super.paintComponent(g)
        for (i <- 0 to 5; j <- 0 to 6) {
          currState(i)(j) match {
            case null => g.setColor(Color.white)
            case "1" => g.setColor(Color.RED)
            case "2" => g.setColor(Color.yellow)
          }
          g.drawOval(j * 63 + 5, i * 63 + 8, 60, 60)
          g.fillOval(j * 63 + 5, i * 63 + 8, 60, 60)
          // g.setColor(PLAYER_1_COLOR)
        }
      }

      setBackground(Color.blue)
    }
    // Create the main panel and add the components
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createColPanelReference(), BorderLayout.NORTH)
      add(gamePanel, BorderLayout.CENTER)
      setBounds(525, 100, 450, 425)
    }
    collectingPanel
  }

  def createMainFrame(welcomeLabel: JLabel, gamePanel: JPanel) = {
    val mainPanel = new JPanel(new BorderLayout()) {
      setBackground(new Color(168, 76, 162))
      add(welcomeLabel, BorderLayout.NORTH)
      add(gamePanel, BorderLayout.CENTER)
      setLayout(null)
    }
    new JFrame("Connect 4") {
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setLocation(-10,0)
      setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
      add(mainPanel)
      pack()
      setVisible(true)
      setAlwaysOnTop(true)
    }
  }

  def updateFrame(welcomeLabel: JLabel, buttonPanel: JPanel) = {
    val mainPanel = new JPanel(new BorderLayout()) {
      setBackground(new Color(168, 76, 162))
      add(welcomeLabel, BorderLayout.NORTH)
      add(buttonPanel, BorderLayout.CENTER)
      setLayout(null)
    }
    val f = getMainFrame("Connect 4")
    f.add(mainPanel)
    f.pack()
    f.setVisible(true)
  }

  def getMainFrame(title: String): JFrame = {
    val windows: Array[Window] = Window.getWindows
    var frame: JFrame = null
    for (w <- windows) {
      w match {
        case f: javax.swing.JFrame if f.getTitle == title => frame = f
        case _ =>
      }
    }
    if (frame != null) frame.getContentPane.removeAll()
    frame
  }

  if (getMainFrame("Connect 4") == null) {
    createMainFrame(createLabel("Welcome to Connect 4 !"), createGamePanel())
  }
  else {
    updateFrame(createLabel("Welcome to Connect 4 !"), createGamePanel())
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