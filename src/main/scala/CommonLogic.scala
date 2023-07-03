import java.awt.*
import java.awt.event.*
import java.io.File
import javax.sound.sampled.{AudioSystem, Clip}
import javax.swing.*

object winnerStatus extends Enumeration {
  //easy 30 medium 45 difficult 56
  val win = "w"; val solved = "s"; val noSol = "n"; val noWin = "o"; val full = "f"
}

def splitString(str: String): Array[String] =  str.split("\\s+")

val isSame = (x: Int, y: Int) => {
  val isEven = (a: Int) => a % 2 == 0
  (isEven(x) && isEven(y)) || (!isEven(x) && !isEven(y))
}

//drawer common
def createLabel(text: String): JLabel = {
  new JLabel(text) {
    setFont(new Font("MV Boli", Font.BOLD, 30))
    setVerticalTextPosition(SwingConstants.CENTER)
    setHorizontalTextPosition(SwingConstants.CENTER)
    setVerticalAlignment(SwingConstants.CENTER)
    setHorizontalAlignment(SwingConstants.CENTER)
    setOpaque(false)
    setForeground(Color.BLACK)
    setBackground(new Color(20, 55, 143))
    setBounds(500, 20, 500, 50)
  }
}

def createMainFrame(welcomeLabel: JLabel, buttonsPanel:JPanel, gamePanel: JPanel, title: String, turnLabel: JLabel):JFrame = {
  val mainPanel = new JPanel(new BorderLayout()) {
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
    add(welcomeLabel, BorderLayout.NORTH)
    add(turnLabel)
    add(buttonsPanel)
    add(gamePanel, BorderLayout.CENTER)
    setLayout(null)
  }
  new JFrame(title) {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocation(-10, 0)
    setExtendedState(Frame.MAXIMIZED_BOTH)
    add(mainPanel)
    pack()
    setVisible(true)
    setAlwaysOnTop(true)
  }
}

def updateFrame(welcomeLabel: JLabel, buttonsPanel:JPanel, gamePanel: JPanel, title: String, turnLabel: JLabel) = {
  val mainPanel = new JPanel(new BorderLayout()) {
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
    add(welcomeLabel, BorderLayout.NORTH)
    add(turnLabel)
    add(buttonsPanel)
    add(gamePanel, BorderLayout.CENTER)
    setLayout(null)
  }
  val f = getMainFrame(title)
  f.add(mainPanel)
//  f.setExtendedState(Frame.MAXIMIZED_BOTH)
//  f.pack()
  f.setVisible(true)

}

def getMainFrame(title: String): JFrame = {
  val windows: Array[Window] = Window.getWindows
  var frame: JFrame = null
  for (w <- windows) {
    w match {
      case f: javax.swing.JFrame if f.getTitle == title =>
        frame = f
      case _ =>
    }
  }
  if (frame != null) frame.getContentPane.removeAll()
  frame
}

def generateAudio(path:String)={
  try {
    val audioInputStream = AudioSystem.getAudioInputStream(new File(path))
    val clip: Clip = AudioSystem.getClip()
    clip.open(audioInputStream)
    clip.start()
    audioInputStream.close()
  } catch {
    case ex: Exception => ex.printStackTrace()
  }
}

def createTurnLabel(turn: Boolean, winner:String, colors: (Color,Color,Color)): JLabel = {
  if (winner != winnerStatus.noWin) {
    val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/".concat(if (winner == winnerStatus.full) "failure3.wav" else "win3.wav")
    generateAudio(path)
  }
  new JLabel {
    setFont(new Font("MV Boli", Font.BOLD, 35))
    setVerticalTextPosition(SwingConstants.CENTER)
    setHorizontalTextPosition(SwingConstants.CENTER)
    setVerticalAlignment(SwingConstants.CENTER)
    setHorizontalAlignment(SwingConstants.CENTER)
//    setOpaque(true)
//    setBackground(new Color(225, 90, 202))
    if (turn) {
      winner match
        case winnerStatus.noWin =>
          setText("Player 1 turn")
          setForeground(colors._1)
        case winnerStatus.full =>
          setText("Tie")
          setForeground(colors._3)
        case _ =>
          setText("Winner is Player 2")
          setForeground(colors._2)
    } else {
      winner match
        case winnerStatus.noWin =>
          setText("Player 2 turn")
          setForeground(colors._2)
        case winnerStatus.full =>
          setText("Tie")
          setForeground(colors._3)
        case _ =>
          setText("Winner is Player 1")
          setForeground(colors._1)
    }
    setBounds(580, 150, 340, 70)
  }
}

def isFull(rows: Int, cols: Int, currState: Array[Array[String]]): Boolean = {
  (0 until rows).flatMap { row =>
    (0 until cols).map { col =>
      if (currState(row)(col) == null) return false
    }
  }
  true
}

//s --> solved (by prolog)  n --> no solution by prolog   w --> you wins
def createSingleAgentLabel(text:String, colors: (Color,Color)): JLabel = {
  val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/"
  if (text == winnerStatus.solved || text == winnerStatus.win) {
    generateAudio(path+"win3.wav" )
  }else if(text == winnerStatus.noSol) {
    generateAudio(path+"failure3.wav" )
  }
  new JLabel {
    setFont(new Font("MV Boli", Font.BOLD, 35))
    setVerticalTextPosition(SwingConstants.CENTER)
    setHorizontalTextPosition(SwingConstants.CENTER)
    setVerticalAlignment(SwingConstants.CENTER)
    setHorizontalAlignment(SwingConstants.CENTER)
    text match{
      case winnerStatus.win =>
        setText("Great Job")
        setForeground(colors._1)
      case winnerStatus.solved =>
        setText("Solved Successfully")
        setForeground(colors._1)
      case winnerStatus.noSol =>
        setText("There is no Solution :(")
        setForeground(colors._2)
      case _ =>
    }
    setBounds(540, 150, 420, 70)
  }
}

def createOptionButtonDA(text: String, frameName: String): (JButton) = {
  new JButton() {
    setText(text)
    setFont(new Font("MV Boli", Font.BOLD, 20))
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
    addActionListener(new ActionListener() {
      def actionPerformed(e: ActionEvent): Unit = {
        val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/click.wav"
        generateAudio(path)
        text match {
          case "Main Menu" =>
            val frame = getMainFrame(frameName)
            if (frame != null) frame.dispose()
            firstInput = (-1, -1)
            initialScreen()
          case _ =>

            frameName match{
              case "Checkers" =>
                firstInput = (-1,-1)
                checkersDrawers((initialCheckersGrid(), true), winnerStatus.noWin)
              case "Chess" =>
                firstInput = (-1, -1)
                val grid: Array[Array[String]] =
                  Array(Array("2♜", "2♞", "2♝", "2♛", "2♚", "2♝", "2♞", "2♜"),
                    Array("2♟", "2♟", "2♟", "2♟", "2♟", "2♟", "2♟", "2♟"),
                    Array("", "", "", "", "", "", "", ""),
                    Array("", "", "", "", "", "", "", ""),
                    Array("", "", "", "", "", "", "", ""),
                    Array("", "", "", "", "", "", "", ""),
                    Array("1♟", "1♟", "1♟", "1♟", "1♟", "1♟", "1♟", "1♟"),
                    Array("1♜", "1♞", "1♝", "1♛", "1♚", "1♝", "1♞", "1♜"))
                chessDrawer((grid, true), winnerStatus.noWin)
              case "Tic Tac Toe" =>
                ticTacToeDrawer((Array.ofDim[String](3, 3), true), winnerStatus.noWin)
              case _ =>
                connect4Drawer((Array.ofDim[String](6, 7), true), winnerStatus.noWin)
            }
        }
      }
    })
  }
}

def createButtonsPanelDA(frameName: String): JPanel = {
  new JPanel(new GridLayout(2, 1, 0, 0)) {
    setBorder(BorderFactory.createLineBorder(Color.BLACK, 2))
    setBounds(250, 230, 200, 150)
    setOpaque(false)
    add(createOptionButtonDA("Main Menu", frameName))
    add(createOptionButtonDA("New Game", frameName))
  }
}

def createButtonsPanelSA(frameName: String, winner: String, currState: Array[Array[String]]): JPanel = {
  new JPanel(new GridLayout(if(frameName == "8 Queens") 4 else 5, 1, 0, 0)) {
    setBounds(250, 230, 200, if(frameName == "8 Queens") 300 else 300)
    setBorder(BorderFactory.createLineBorder(Color.BLACK, 2))
    setBackground(Color.black)
    setOpaque(true)
    add(createOptionButtonSA("Main Menu", frameName, winner, currState))
    if(frameName == "8 Queens") add(createOptionButtonSA("New Game", frameName, winner, currState))
    else{
      add(createOptionButtonSA("Easy", frameName, winner, currState))
      add(createOptionButtonSA("Medium", frameName, winner, currState))
      add(createOptionButtonSA("Hard", frameName, winner, currState))
    }
    if(frameName == "8 Queens") add(createOptionButtonSA("Remove queen", frameName, winner, currState))
    add(createOptionButtonSA("Solve", frameName, winner, currState))
  }
}
def createOptionButtonSA(text: String, frameName: String, winner: String, currState: Array[Array[String]]): (JButton) = {
  new JButton() {
    setText(text)
    setFont(new Font("MV Boli", Font.BOLD, 20))
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
        val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/click.wav"
        text match {
          case "Main Menu" =>
            generateAudio(path)
            isRemove = false
            val frame = getMainFrame(frameName)
            if (frame != null) frame.dispose()
            initialScreen()
          case "Remove queen" =>
            if (winner == winnerStatus.noSol || winner == winnerStatus.noWin) {
              generateAudio(path)
              isRemove = true
            }
          case "Solve" =>
            if (winner == winnerStatus.noSol || winner == winnerStatus.noWin) {
              generateAudio(path)
              isRemove = false
              if(frameName == "8 Queens") {
                val res = eightQueensController(currState, "solve")
                eightQueensDrawer(res._1, res._2)
              } else {
                val res = sudokuController(currState, "solve")
                sudokuDrawer(res._1, res._2)
              }
            }
          case "New Game" =>
            generateAudio(path)
            isRemove = false
            eightQueensDrawer(Array.ofDim[String](8, 8), winnerStatus.noWin)
          case "Easy" =>
          //easy 30 medium 45 difficult 56
            generateAudio(path)
            sudokuDrawer(generateInitialSudoku(sudokuLevel.EASY), winnerStatus.noWin)
          case "Medium" =>
            generateAudio(path)
            sudokuDrawer(generateInitialSudoku(sudokuLevel.MEDIUM), winnerStatus.noWin)
          case _ =>
            generateAudio(path)
            sudokuDrawer(generateInitialSudoku(sudokuLevel.HARD), winnerStatus.noWin)
        }
      }
    })
  }
}


