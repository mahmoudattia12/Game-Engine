import scala.util.control.Breaks.{break, breakable}
import javax.swing.*
import java.awt.*
import java.awt.event.*
def initialCheckersGrid(): Array[Array[String]] = {
  val temp: Array[Array[String]] = Array.ofDim[String](8, 8)
  (0 to 2).flatMap { row =>
    (0 to 7).map { col =>
      if (!isSame(row, col)) temp(row)(col) = "1"
    }
  }
  (5 to 7).flatMap { row =>
    (0 to 7).map { col =>
      if (!isSame(row, col)) temp(row)(col) = "2"
    }
  }
  temp
}

def createInitButton(text:String, frame:JFrame):JButton = {
  val button = new JButton() {
    setText(text)
    setFont(new Font("MV Boli", Font.BOLD, 30))
    setVerticalTextPosition(SwingConstants.CENTER)
    setHorizontalTextPosition(SwingConstants.CENTER)
    setVerticalAlignment(SwingConstants.CENTER)
    setHorizontalAlignment(SwingConstants.CENTER)
    setOpaque(true)
    setBackground(Color.decode("#be32f0"))
    setForeground(Color.black)
    setFocusable(false)
    setBorder(BorderFactory.createLineBorder(Color.BLACK, 2))

    //set hover
    addMouseListener(new MouseAdapter() {
      override def mouseEntered(e: MouseEvent): Unit = {
        setBackground(new Color(232, 126, 251))
      }

      override def mouseExited(e: MouseEvent): Unit = {
        setBackground(Color.decode("#be32f0"))
        setForeground(Color.black)
      }
    })

    //set action
    addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent): Unit = {
        val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/interface-click-tone-2568.wav"
        generateAudio(path)
        text match {
          case "Checker" =>
          case "Chess" =>
          case "Tic Tac Toe" =>
            ticTacToeDrawer((Array.ofDim[String](3, 3), true), "none")
          case "Connect 4" =>
            connect4Drawer((Array.ofDim[String](6, 7), true), "none")
          case "Sudoku" =>
          case _ =>
            eightQueensDrawer(Array.ofDim[String](8, 8), "none")
        }
        frame.dispose()
      }
    })
  }
  button
}

def createOptionsPanel(frame: JFrame): JPanel = {
  new JPanel(new GridLayout(6, 1)) {
    add(createInitButton("Checker", frame))
    add(createInitButton("Chess", frame))
    add(createInitButton("Tic Tac Toe", frame))
    add(createInitButton("Connect 4", frame))
    add(createInitButton("Sudoku", frame))
    add(createInitButton("8 Queens", frame))
    setBounds(500, 230, 500, 500)
    setBorder(BorderFactory.createLineBorder(Color.BLACK, 2))
    setOpaque(true)
    setBackground(Color.black)
  }
}

def initialScreen() = {

  val frame = new JFrame("Game Engine") {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocation(-10, 0)
    setExtendedState(Frame.MAXIMIZED_BOTH)


  }
  val mainPanel = new JPanel() {
    override def paintComponent(g: Graphics): Unit = {
      super.paintComponent(g)
      val g2d = g.asInstanceOf[Graphics2D]

      val width = getWidth
      val height = getHeight

      val startColor = Color.decode("#43c6ac")
      val endColor = Color.decode("#191654")
      val gradientPaint = new GradientPaint(0, 0, startColor, width, height, endColor)
      g2d.setPaint(gradientPaint)
      g2d.fillRect(0, 0, width, height)
    }

    add(createLabel("Welcome to Our Game Engine"))
    add(createOptionsPanel(frame))
    setLayout(null)
  }
  frame.add(mainPanel)
  frame.pack()
  frame.setVisible(true)
  frame.setAlwaysOnTop(true)
}




//  val isBetweenOneAndSix = (str: String) => str.length == 1 && str.charAt(0).isDigit && str.charAt(0) >= '1' && str.charAt(0) <= '6'
//  println("\t\t\t\t\t\t\t\t\t\t\tWelcome to Our Game Engine\n")
//  println("1- Checkers\t\t2- Chess\t\t3- Tic-Tac-Toe\t\t4- Connect 4\t\t5- Sudoku\t\t6- Eight Queens")
//  var gameChoice: String = "1"
//  breakable {
//    while (true) {
//      scala.Predef.print("please enter your choice(1/2/3/4/5/6): ")
//      gameChoice = scala.io.StdIn.readLine()
//      if (isBetweenOneAndSix(gameChoice)) break
//      else {
//        println("invalid input!!!")
//      }
//    }
//  }
//  gameChoice match {
//    case "1" =>
////      gameEngine(checkersDrawers, initialCheckersGrid())
//    case "2" =>
//      val grid: Array[Array[String]] =
//        Array(Array("2♜", "2♞", "2♝", "2♛", "2♚", "2♝", "2♞", "2♜"),
//        Array("2♟", "2♟", "2♟", "2♟", "2♟", "2♟", "2♟", "2♟"),
//        Array("", "", "", "", "", "", "", ""),
//        Array("", "", "", "", "", "", "", ""),
//        Array("", "", "", "", "", "", "", ""),
//        Array("", "", "", "", "", "", "", ""),
//        Array("1♟", "1♟", "1♟", "1♟", "1♟", "1♟", "1♟", "1♟"),
//        Array("1♜", "1♞", "1♝", "1♛", "1♚", "1♝", "1♞", "1♜"))
////      gameEngine(chessDrawer, grid)
//    case "3" =>
//      ticTacToeDrawer((Array.ofDim[String](3, 3), true), "none")
//    case "4" =>
//      connect4Drawer((Array.ofDim[String](6, 7), true), "none")
//    case "5" =>
////      gameEngine(sudokuDrawer, generateInitialSudoku())
//    case _ =>
//      eightQueensDrawer(Array.ofDim[String](8, 8), "none")
////      gameEngine(eightQueensDrawer, Array.ofDim[String](8, 8))
//  }
