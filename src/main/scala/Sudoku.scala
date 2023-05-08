import javax.swing._
import java.awt._

def sudokuDrawer(currState: Array[Array[String]]): Unit = {
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
    new JLabel(currState(row)(col)) {
      setFont(new Font("Arial", Font.BOLD, 20))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)

      setBorder(BorderFactory.createLineBorder(Color.black))
//      setBackground(Color.white)
//      if(currState(row)(col) != null)
      if(currState(row)(col) != null && currState(row)(col)(0) == '0'){
        setText(currState(row)(col)(1).toString)
        setForeground(Color.black)
      }
      else {
        setText(currState(row)(col))
        setForeground(Color.blue)
      }
      setOpaque(true)
    }
  }

  def createBoxPanel(dx: Int, dy: Int): JPanel = {
    new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 2))
      for {
        row <- dx * 3 until dx * 3 + 3
        col <- dy * 3 until dy * 3 + 3
      } {
        add(createCellLabel(row, col))
      }
    }
  }

  def createRowPanelReference(): JPanel = {
    val rowPanel = new JPanel(new GridLayout(9, 1)) {
      setPreferredSize(new Dimension(40, 300))
      setBackground(new Color(168, 76, 162))
    }
    for (row <- 1 to 9) {
      val label = createLabel((10 - row).toString)
      label.setForeground(new Color(239, 196, 67))
      label.setBackground(new Color(168, 76, 162))
      label.setHorizontalAlignment(SwingConstants.CENTER)
      rowPanel.add(label)
    }
    rowPanel
  }

  def createColPanelReference(): JPanel = {
    val colPanel = new JPanel(new GridLayout(1, 10)) {
      setPreferredSize(new Dimension(400, 50))
      setBackground(new Color(168, 76, 162))
    }
    for (col <- Array(" ", " a", " b", " c", " d", " e", " f", " g", " h", " i")) {
      val label = createLabel(col)
      label.setBackground(new Color(168, 76, 162))
      label.setForeground(new Color(239, 196, 67))
      label.setVerticalAlignment(SwingConstants.NORTH)
      label.setHorizontalAlignment(SwingConstants.CENTER)
      colPanel.add(label)
    }
    colPanel
  }
  def createGamePanel(currState: Array[Array[String]]): JPanel = {
    val gamePanel = new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 4))
      for {
        row <- 0 until 3
        col <- 0 until 3
      } {
        add(createBoxPanel(row, col))
      }
    }
    val collectingPanel = new JPanel(new BorderLayout()) {
      add(createRowPanelReference(), BorderLayout.WEST)
      add(gamePanel, BorderLayout.CENTER)
      add(createColPanelReference(), BorderLayout.SOUTH)
      setBounds(525, 130, 400, 400)
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
    new JFrame("Sudoku") {
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
    val f = getMainFrame("Sudoku")
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
  if (getMainFrame("Sudoku") == null) {
    createMainFrame(createLabel("Welcome to Sudoku!"), createGamePanel(currState))
  }
  else {
    updateFrame(createLabel("Welcome to Sudoku!"), createGamePanel(currState))
  }
}



def sudokuController(currState: (Array[Array[String]], Boolean), input: String): (Boolean, Array[Array[String]]) = {
  //function to rephrase cellInput
  def splitString(str: String): Array[String] = {
    str.split("\\s+")
  }
  val inputArr = splitString(input)
  //  val cellInput = inputArr(0)
  //  val value = inputArr(1)
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
      case 'i' => 8
      case _ => -1
    }
  }

  def getRow(c: Char): Int = {
    val isWithinRange = (value: Int, min: Int, max: Int) => value >= min && value <= max
    val row: Int = 9 - (c.toInt - '0'.toInt)
    isWithinRange(row, 0, 8) match {
      case true => row
      case _ => -1
    }
  }

  val rephrase = (str: String) => if(str.length == 2) (getRow(str(0)), getCol(str(1))) else (-1,-1)


  //x = index._1/3 * 3, y = index._2/3 *3
  def validateBlock(x: Int, y: Int, cell: String): Boolean = {
    (x until x + 3).flatMap { row =>
      (y until y + 3).map { col =>
        if ((currState._1(row)(col)!=null && currState._1(row)(col).length == 2 && currState._1(row)(col)(1).toString == inputArr(1))
          || (currState._1(row)(col) == inputArr(1))) return false
      }
    }
    true
  }

  def validateRow(index: (Int, Int), cell: String): Boolean = {
    LazyList.from(0).takeWhile { case (j) => j <= 8 }.foreach { case (j) =>
      if ((currState._1(index._1)(j)!=null && currState._1(index._1)(j).length == 2 && currState._1(index._1)(j)(1).toString == inputArr(1))
        || (currState._1(index._1)(j) == inputArr(1))) return false
    }
    true
  }

  def validateCol(index: (Int, Int), cell: String): Boolean = {
    LazyList.from(0).takeWhile { case (i) => i <= 8 }.foreach { case (i) =>
      if ((currState._1(i)(index._2) != null && currState._1(i)(index._2).length == 2 &&currState._1(i)(index._2)(1).toString == inputArr(1))
        || ( currState._1(i)(index._2) == inputArr(1))  ) return false
    }
    true
  }

  def isValid(index: (Int, Int), cell: String): Boolean = {
    if (currState._1(index._1)(index._2) == null) {
      return validateRow(index, cell) && validateCol(index, cell) && validateBlock(index._1 / 3 * 3, index._2 / 3 * 3, cell)
    }
    false
  }


  //function to validate and set the cell in the currState
  def setCell(index: (Int, Int), cell: String): Boolean = {
    if (isValid(index, cell)) {
      currState._1(index._1)(index._2) = cell;
      true
    } else false
  }
  def removeCell(index:(Int,Int)): Boolean = {
    if(currState._1(index._1)(index._2) != null && currState._1(index._1)(index._2)(0) != '0'){
      currState._1(index._1)(index._2) = null;
      true
    }else false
  }

  if(inputArr.length == 2){
    if (inputArr(0) == "remove") {
      val cell = rephrase(inputArr(1))
      cell match {
        case (-1, -1) => (false, currState._1)
        case _ =>
          (removeCell(cell), currState._1)
      }
    } else {
      val isBetweenOneAndNine = (str: String) => str.length == 1 && str.charAt(0).isDigit && str.charAt(0) >= '1' && str.charAt(0) <= '9'
      if (isBetweenOneAndNine(inputArr(1))) {
        val cell = rephrase(inputArr(0))
        cell match {
          case (-1, _) => (false, currState._1)
          case (_, -1) => (false, currState._1)
          case _ =>
            (setCell(cell, inputArr(1)), currState._1)
        }
      } else (false, currState._1)
    }
  }else (false, currState._1)


}