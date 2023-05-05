import java.awt.*
import javax.swing.*
import scala.annotation.tailrec

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
      if(text != null) setText("\uD83D\uDD34") else setText(null)
      setForeground(colorF)
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
        val foreColor: Color = if (currState(row)(col) == "1") Color.red else  Color.blue
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
  val rephrase = (str: String) => if(str.length == 2) (getRow(str(0)), getCol(str(1))) else (-1,-1)

  def validate(player: Int, oldi: Int, oldj: Int, newi: Int, newj: Int): String = {
    val validSource =
      if(player == 1)
        currState._1(oldi)(oldj) == "1"
      else
        currState._1(oldi)(oldj) == "2"

    val moveIsValid =
      if (player == 1)
        newi - oldi == 1 && Math.abs(newj - oldj) == 1
      else
        oldi - newi == 1 && Math.abs(newj - oldj) == 1

    if (validSource && moveIsValid) {
      currState._1(newi)(newj) match {
        case null => "just move"
        case "1" =>
          if (player == 2) "check for jmp" else "not valid"
        case "2" =>
          if (player == 1) "check for jmp" else "not valid"
        case _ => "not valid"
      }
    } else {
      "not valid"
    }
  }

  def isWithin(pos: (Int, Int)): Boolean = {
    val (row, col) = pos
    row >= 0 && row < currState._1.length && col >= 0 && col < currState._1(0).length
  }
  
  def checkForJmp(i: Int, j: Int, currentTurn: Int): String = {
    if (currentTurn == 1) {
      val rightChild = (i + 1, j + 1)
      val leftChild = (i + 1, j - 1)
      if (isWithin(rightChild) && currState._1(rightChild._1)(rightChild._2) == "2") {
        val rightOfRightChild = (i + 2, j + 2)
        if (isWithin(rightOfRightChild) && currState._1(rightOfRightChild._1)(rightOfRightChild._2) == null) {
          return "right jmp"
        }
      }
      if (isWithin(leftChild) && currState._1(leftChild._1)(leftChild._2) == "2") {
        val leftOfLeftChild = (i + 2, j - 2)
        if (isWithin(leftOfLeftChild) && currState._1(leftOfLeftChild._1)(leftOfLeftChild._2) == null) {
          return "left jmp"
        }
      }
      null // return null as a String
    } else {
      val rightChild = (i - 1, j + 1)
      val leftChild = (i - 1, j - 1)
      if (isWithin(rightChild) && currState._1(rightChild._1)(rightChild._2) == "1") {
        val rightOfRightChild = (i - 2, j + 2)
        if (isWithin(rightOfRightChild) && currState._1(rightOfRightChild._1)(rightOfRightChild._2) == null) {
          return "right jmp"
        }
      }
      if (isWithin(leftChild) && currState._1(leftChild._1)(leftChild._2) == "1") {
        val leftOfLeftChild = (i - 2, j - 2)
        if (isWithin(leftOfLeftChild) && currState._1(leftOfLeftChild._1)(leftOfLeftChild._2) == null) {
          return "left jmp"
        }
      }
      null // return null as a String
    }
  }

  @tailrec
  def jmpRecursively(i: Int, j: Int, turn: Int): Unit = {
    if (!isWithin((i, j))) return
    val check = checkForJmp(i, j, turn)
    println("check from recur : " + check)
    if(turn == 1){
      check match {
        case "right jmp" =>
          println("right jump")
          currState._1(i)(j) = null
          currState._1(i + 1)(j + 1) = null
          currState._1(i + 2)(j + 2) = "1"
          jmpRecursively(i + 2, j + 2, turn)
        case "left jmp" =>
          println("left jump")
          currState._1(i)(j) = null
          currState._1(i + 1)(j - 1) = null
          currState._1(i + 2)(j - 2) = "1"
          jmpRecursively(i + 2, j - 2, turn)
        case _ =>
          println("ff")
      }
    }else{
      check match {
        case "right jmp" =>
          println("right jump")
          currState._1(i)(j) = null
          currState._1(i - 1)(j + 1) = null
          currState._1(i - 2)(j + 2) = "2"
          jmpRecursively(i - 2, j + 2, turn)
        case "left jmp" =>
          println("left jump")
          currState._1(i)(j) = null
          currState._1(i - 1)(j - 1) = null
          currState._1(i - 2)(j - 2) = "2"
          jmpRecursively(i - 2, j - 2, turn)
        case _ =>
          println("ff")
    }


    }
  }

  if(inputArr.length == 2){
    val from = rephrase(inputArr(0))
    val to = rephrase(inputArr(1))

    (from, to) match {
      case ((-1, _),(_,_)) => (false, currState._1)
      case ((_, -1),(_,_)) =>  (false, currState._1)
      case ((_, _),(-1,_)) =>  (false, currState._1)
      case ((_, _),(_,-1)) =>  (false, currState._1)
      case _ =>
        val moveStatus: String = if (currState._2) validate(1, from._1, from._2, to._1, to._2)
                                else validate(2, from._1, from._2, to._1, to._2)
        if(moveStatus == "not valid"){
          (false, currState._1)
        }else if(moveStatus == "check for jmp"){
          val check = if (currState._2) checkForJmp(from._1, from._2, 1)
                      else checkForJmp(from._1, from._2, 2)
          if(check == null){
            return (false,currState._1)
          }
          if(currState._2){
            jmpRecursively(from._1, from._2, 1)
          }  else {
            jmpRecursively(from._1, from._2, 2)
          }
          currState._1.foreach(row => {
            row.foreach(element => scala.Predef.print(s"$element "))
            println()
          })
          (true, currState._1)
        }else{
          currState._1(from._1)(from._2) =  null
          if(currState._2) currState._1(to._1)(to._2) = "1"
          else currState._1(to._1)(to._2) = "2"
          (true, currState._1)
        }
    }
  }else (false,currState._1)

}

