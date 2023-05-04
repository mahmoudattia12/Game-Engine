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
      setBackground(Color.white)
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
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 1))
      for {
        row <- dx * 3 until dx * 3 + 3
        col <- dy * 3 until dy * 3 + 3
      } {
        add(createCellLabel(row, col))
      }
    }
  }

  def createGamePanel(currState: Array[Array[String]]): JPanel = {
    new JPanel(new GridLayout(3, 3)) {
      setBorder(BorderFactory.createLineBorder(Color.BLACK, 3))
      setBounds(600, 150, 300, 300)
      for {
        row <- 0 until 3
        col <- 0 until 3
      } {
        add(createBoxPanel(row, col))
      }
    }
  }

  def createMainFrame(welcomeLabel: JLabel, buttonPanel: JPanel) = {
    val mainPanel = new JPanel(new BorderLayout()) {
      setBackground(new Color(238, 162, 226))
      welcomeLabel.setBounds(600, 20, 300, 30)
      buttonPanel.setBounds(600, 150, 300, 300)
      add(welcomeLabel, BorderLayout.NORTH)
      add(buttonPanel, BorderLayout.CENTER)
      setLayout(null)
    }
    new JFrame("Sudoku") {
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
      add(mainPanel)
      pack()
      setVisible(true)
    }
  }

  createMainFrame(createLabel("Welcome to Sudoku!"), createGamePanel(currState))
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
        if (currState._1(row)(col) == inputArr(1)) return false
      }
    }
    true
  }

  def validateRow(index: (Int, Int), cell: String): Boolean = {
    LazyList.from(0).takeWhile { case (j) => j <= 8 }.foreach { case (j) =>
      if (currState._1(index._1)(j) == inputArr(1)) return false
    }
    true
  }

  def validateCol(index: (Int, Int), cell: String): Boolean = {
    LazyList.from(0).takeWhile { case (i) => i <= 8 }.foreach { case (i) =>
      if (currState._1(i)(index._2) == inputArr(1)) return false
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