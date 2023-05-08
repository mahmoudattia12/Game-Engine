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

  def createRowPanelReference(): JPanel = {
    val rowPanel = new JPanel(new GridLayout(8, 1)) {
      setPreferredSize(new Dimension(40, 300))
      setBackground(new Color(168, 76, 162))
    }
    for (row <- 1 to 8) {
      val label = createLabel((9 - row).toString)
      label.setForeground(new Color(239, 196, 67))
      label.setBackground(new Color(168, 76, 162))
      label.setHorizontalAlignment(SwingConstants.CENTER)
      rowPanel.add(label)
    }
    rowPanel
  }

  def createColPanelReference(): JPanel = {
    val colPanel = new JPanel(new GridLayout(1, 9)) {
      setPreferredSize(new Dimension(400, 50))
      setBackground(new Color(168, 76, 162))
    }
    for (col <- Array(" ", "a ", "b", "c", "d", "e", "f", "g", "h")) {
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
    val gamePanel = new JPanel(new GridLayout(8, 8)) {
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
    }
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createRowPanelReference(), BorderLayout.WEST)
      add(gamePanel, BorderLayout.CENTER)
      add(createColPanelReference(), BorderLayout.SOUTH)
      setBounds(530, 100, 400, 400)
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
    new JFrame("8 Queens") {
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
    val f = getMainFrame("8 Queens")
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

  if (getMainFrame("8 Queens") == null) {
    createMainFrame(createLabel("Welcome to 8 Queens!"), createGamePanel())
  }
  else {
    updateFrame(createLabel("Welcome to 8 Queens!"), createGamePanel())
  }
}
def eightQueensController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  def splitString(str: String): Array[String] = {
    str.split("\\s+")
  }
  val inputArr = splitString(input)

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

  val rephrase = (str: String) => if(str.length == 2) (getRow(str(0)), getCol(str(1))) else (-1,-1)

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

  def removeCell(index: (Int, Int)): Boolean = {
    if(currState._1(index._1)(index._2) == "♛"){
      currState._1(index._1)(index._2) = null; true
    }else
      false
  }
  if(inputArr(0) == "remove"){
    val cell = rephrase(inputArr(1))
    cell match {
      case (_, -1) => (false, currState._1)
      case (-1, _) => (false, currState._1)
      case _ =>
        (removeCell(cell), currState._1)
    }
  }else{
    val cell = rephrase(input)
    cell match {
      case (_, -1) => (false, currState._1)
      case (-1, _) => (false, currState._1)
      case _ =>
        (setCell(cell), currState._1)
    }
  }

}
