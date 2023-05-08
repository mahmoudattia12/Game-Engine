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
      setForeground(Color.white)
      setBackground(new Color(20, 55, 143))
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
  def createRowPanelReference(): JPanel ={
    val rowPanel = new JPanel(new GridLayout(3, 1)) {
      setPreferredSize(new Dimension(100, 300))
      setBackground(new Color(168, 76, 162))
    }
    for (row <- 1 to 3) {
      val label = createLabel((4 - row).toString)
      label.setHorizontalAlignment(SwingConstants.CENTER)
      label.setBackground(new Color(168, 76, 162))
      label.setForeground(new Color(239, 196, 67))
      rowPanel.add(label)
    }
    rowPanel
  }
  def createColPanelReference(): JPanel = {
    val colPanel = new JPanel(new GridLayout(1, 4)) {
      setPreferredSize(new Dimension(300, 100))
      setBackground(new Color(168, 76, 162))
    }
    for (col <- Array(" ", "a", "b", "c")) {
      val label = createLabel(col)
      label.setBackground(new Color(168, 76, 162))
      label.setForeground(new Color(239, 196, 67))
      label.setVerticalAlignment(SwingConstants.NORTH)
      label.setHorizontalAlignment(SwingConstants.CENTER)
      colPanel.add(label)
    }
    colPanel
  }
  def createGamePanel(): JPanel = {
    val gamePanel = new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.YELLOW, 3))
    }
    for {
      row <- 0 until 3
      col <- 0 until 3
    } {
      gamePanel.add(createCellLabel(row, col))
    }

    // Create the main panel and add the components
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createRowPanelReference(), BorderLayout.WEST)
      add(gamePanel, BorderLayout.CENTER)
      add(createColPanelReference(), BorderLayout.SOUTH)
      setBounds(500, 150, 400, 400)
    }
    collectingPanel
  }
  def createMainFrame(welcomeLabel: JLabel, buttonPanel: JPanel) = {
    val mainPanel = new JPanel(new BorderLayout()) {
      setBackground(new Color(168, 76, 162))
      add(welcomeLabel, BorderLayout.NORTH)
      add(buttonPanel, BorderLayout.CENTER)
      setLayout(null)
    }
    new JFrame("Tic Tac Toe"){
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
    val f = getMainFrame("Tic Tac Toe")
    f.add(mainPanel)
    f.pack()
    f.setVisible(true)


  }
  def getMainFrame(title: String): JFrame ={
    val windows : Array[Window] = Window.getWindows
    var frame: JFrame = null
    for(w <- windows){
      w match{
        case f:javax.swing.JFrame if f.getTitle == title => frame = f
        case _ =>
      }
    }
    if(frame != null) frame.getContentPane.removeAll()
    frame
  }
  if(getMainFrame("Tic Tac Toe") == null) {
    createMainFrame(createLabel("Welcome to Tic Tac Toe!"), createGamePanel())
  }
  else{
    updateFrame(createLabel("Welcome to Tic Tac Toe!"), createGamePanel())
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



