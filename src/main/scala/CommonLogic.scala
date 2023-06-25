import java.awt.*
import java.io.File
import javax.sound.sampled.{AudioSystem, Clip}
import javax.swing.*

//functions to rephrase input of 8x8 grids
def getCol_8x8(c: Char): Int = {
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
def getRow_8x8(c: Char): Int = {
  val row: Int = 8 - (c.toInt - '0'.toInt)
  isWithinRange(row, 0, 7) match {
    case true => row
    case _ => -1
  }
}
val rephrase_8x8 = (str: String) => if(str.length == 2) (getRow_8x8(str(0)), getCol_8x8(str(1))) else (-1,-1)

def splitString(str: String): Array[String] =  str.split("\\s+")

val isWithinRange = (value: Int, min: Int, max: Int) => value >= min && value <= max

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

def createRowPanelReference(rows: Int, w: Int): JPanel = {
  val rowPanel = new JPanel(new GridLayout(rows, 1)) {
    setPreferredSize(new Dimension(w, 300))
    setBackground(new Color(168, 76, 162))
  }
  for (row <- 1 to rows) {
    val label = createLabel((rows + 1 - row).toString)
    label.setForeground(new Color(239, 196, 67))
    label.setBackground(new Color(168, 76, 162))
    label.setHorizontalAlignment(SwingConstants.CENTER)
    rowPanel.add(label)
  }
  rowPanel
}

def createColPanelReference(cols: Int, w:Int, h:Int, letters: Array[String]): JPanel = {
  val colPanel = new JPanel(new GridLayout(1, cols)) {
    setPreferredSize(new Dimension(w, h))
    setBackground(new Color(168, 76, 162))
  }
  for (col <- letters) {
    val label = createLabel(col)
    label.setBackground(new Color(168, 76, 162))
    label.setForeground(new Color(239, 196, 67))
    label.setVerticalAlignment(SwingConstants.NORTH)
    label.setHorizontalAlignment(SwingConstants.CENTER)
    colPanel.add(label)
  }
  colPanel
}

def createMainFrame(welcomeLabel: JLabel, buttonsPanel:JPanel, gamePanel: JPanel, title: String, wTurn: Boolean, turnLabel: JLabel) = {
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
//    setBackground(new Color(225, 90, 202))
    add(welcomeLabel, BorderLayout.NORTH)
    if(wTurn) add(turnLabel)
    add(buttonsPanel)
    add(gamePanel, BorderLayout.CENTER)
    setLayout(null)
  }
  new JFrame(title) {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocation(-10, 0)
//    setPreferredSize(new Dimension(Integer.MAX_VALUE, Integer.MAX_VALUE))
    setExtendedState(Frame.MAXIMIZED_BOTH)
    add(mainPanel)
    pack()
    setVisible(true)
    setAlwaysOnTop(true)
  }
}

def updateFrame(welcomeLabel: JLabel, buttonsPanel:JPanel, gamePanel: JPanel, title: String, wTurn: Boolean, turnLabel: JLabel) = {
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
    if(wTurn)add(turnLabel)
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
  if (winner != "none") {
    val path = "D:/CSED/level2/2nd semester/programming paradigms/project/Phase-1/Game-Engine/audio/".concat(if (winner == "full") "failure3.wav" else "win3.wav")
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
        case "none" =>
          setText("Player 1 turn")
          setForeground(colors._1)
        case "full" =>
          setText("Tie")
          setForeground(colors._3)
        case _ =>
          setText("Winner is Player 2")
          setForeground(colors._2)
    } else {
      winner match
        case "none" =>
          setText("Player 2 turn")
          setForeground(colors._2)
        case "full" =>
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
