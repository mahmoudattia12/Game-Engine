import java.awt.*
import javax.swing.*
import scala.annotation.tailrec

def chessDrawer(currState: Array[Array[String]]): Unit = {
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

  def createCellLabel(colorB: Color, colorF: Color, row: Int, col: Int): JLabel = {
    new JLabel() {
      setFont(new Font("Noto Color Emoji", Font.PLAIN, 40))
      setHorizontalAlignment(SwingConstants.CENTER)
      setVerticalAlignment(SwingConstants.CENTER)
      setBackground(colorB)
      setForeground(colorF)
      if(currState(row)(col) != "") setText(currState(row)(col)(1).toString) else setText("")
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
        val foreColor: Color = if (currState(row)(col) != "" && currState(row)(col)(0) == '2') Color.black else Color.white

        if (isSame(row, col)) {
          add(createCellLabel(new Color(225, 210, 156), foreColor, row, col))
        } else {
          add(createCellLabel(new Color(119, 149, 86), foreColor, row, col))
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
    new JFrame("Chess") {
      setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
      add(mainPanel)
      pack()
      setVisible(true)
    }
  }

  createMainFrame(createLabel("Welcome to  Chess!"), createGamePanel())
}
def chessController(currState: (Array[Array[String]], Boolean) , input: String): (Boolean, Array[Array[String]]) = {
  val playerTurn : Int = if(currState._2) 1 else 2
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
    val isWithinRange = (value: Int, min: Int, max: Int) => value >= min && value <= max
    val row: Int = 8 - (c.toInt - '0'.toInt)
    isWithinRange(row, 0, 7) match {
      case true => row
      case _ => -1
    }
  }
  val rephrase = (str: String) => if (str.length == 2) (getRow(str(0)), getCol(str(1))) else (-1, -1)
  //validate pieces
  def pawnCanMove(from: (Int, Int), to: (Int, Int)):Boolean = {
    val direction: Int = if(!currState._2) 1 else -1
    if(from._2 == to._2){
      if (from._1 + direction == to._1 && currState._1(to._1)(to._2) == "")  true // one move
      else if (from._1 + 2 * direction == to._1 && from._1 == (if(!currState._2) 1 else 6) &&
        currState._1(to._1)(to._2) == "" && currState._1(from._1 + direction)(to._2) == "") true   // double move
      else false
    }else if (from._1 + direction == to._1 && Math.abs(from._2 - to._2) == 1 && currState._1(to._1)(to._2) != "") true
    else false
  }

  def kingCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (Math.abs(from._1 - to._1) <= 1 && Math.abs(from._2 - to._2) <= 1) return true else false
  }

  def knightCanMove(from:(Int, Int), to:(Int,Int)): Boolean = {
    if ((Math.abs(from._1 - to._1) == 2 && Math.abs(from._2 - to._2) == 1) ||
      (Math.abs(from._1 - to._1) == 1 && Math.abs(from._2 - to._2) == 2)) return true
    else false
  }

  //used in validate queen&rock&bishop
  def checkDiagonalObstruction(fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val rowDir = if (toR > fromR) 1 else -1
    val colDir = if (toC > fromC) 1 else -1
    val (i, j) = Iterator.iterate((fromR + rowDir, fromC + colDir)) { case (r, c) => (r + rowDir, c + colDir) }
      .takeWhile { case (r, c) => r != toR && c != toC }
      .find { case (r, c) => currState._1(r)(c) != "" }
      .getOrElse((toR, toC))
    i == toR && j == toC
  }
  def checkHorizontalObstruction( fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val colDir = if (toC > fromC) 1 else -1
    val indices = LazyList.iterate(fromC + colDir)(_ + colDir).takeWhile(_ != toC)
    indices.forall(j => currState._1(toR)(j) == "")
  }
  def checkVerticalObstruction(fromR: Int, fromC: Int, toR: Int, toC: Int): Boolean = {
    val rowDir = if (toR > fromR) 1 else -1
    val i = Iterator.from(fromR + rowDir, rowDir)
    !i.takeWhile(_ != toR).exists(r => currState._1(r)(toC) != "")
  }

  def queenCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      checkDiagonalObstruction(from._1, from._2, to._1, to._2)
    }else if(from._1 == to._1){
      checkHorizontalObstruction(from._1, from._2, to._1, to._2)
    }else if(from._2 == to._2){
      checkVerticalObstruction(from._1, from._2, to._1, to._2)
    }else false
  }

  def rockCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (from._1 == to._1) {
      checkHorizontalObstruction(from._1, from._2, to._1, to._2)
    } else if (from._2 == to._2) {
      checkVerticalObstruction(from._1, from._2, to._1, to._2)
    } else false
  }

  def bishopCanMove(from:(Int,Int), to:(Int,Int)): Boolean = {
    if (Math.abs(from._1 - to._1) == Math.abs(from._2 - to._2)) {
      checkDiagonalObstruction(from._1, from._2, to._1, to._2)
    }else false
  }

  def canMove(from:(Int,Int), to:(Int,Int)): Boolean ={
    val piece = currState._1(from._1)(from._2)(1)
    piece match{
      case '♟' => pawnCanMove(from, to)
      case '♚' => kingCanMove(from, to)
      case '♛' => queenCanMove(from, to)
      case '♜' => rockCanMove(from, to)
      case '♝' => bishopCanMove(from, to)
      case _ => knightCanMove(from, to)
    }
  }

  if(inputArr.length == 2) {
    val from = rephrase(inputArr(0))
    val to = rephrase(inputArr(1))
    (from, to) match {
      case ((-1, _), (_, _)) => (false, currState._1)
      case ((_, -1), (_, _)) => (false, currState._1)
      case ((_, _), (-1, _)) => (false, currState._1)
      case ((_, _), (_, -1)) => (false, currState._1)
      case _ =>
        if (from._1 < 0 || from._1 >= 8 || from._2 < 0 || from._2 >= 8
          || to._1 < 0 || to._1 >= 8 || to._2 < 0 || to._2 >= 8) {
          return (false, currState._1)
        }
        if(currState._1(from._1)(from._2) == "" || currState._1(from._1)(from._2)(0).toInt-48 != playerTurn || (currState._1(to._1)(to._2) != "" &&  currState._1(to._1)(to._2)(0).toInt == playerTurn) ){
          (false, currState._1)
        }else{
          if(!canMove(from, to)) {
            (false, currState._1)
          }
          else{
            currState._1(to._1)(to._2) = currState._1(from._1)(from._2)
            currState._1(from._1)(from._2) = ""
            (true, currState._1)
          }
        }
    }
  }else (false, currState._1)
}
