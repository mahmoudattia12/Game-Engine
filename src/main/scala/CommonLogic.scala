import java.awt._
import javax.swing._
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

def createMainFrame(welcomeLabel: JLabel, buttonPanel: JPanel, title: String) = {
  val mainPanel = new JPanel(new BorderLayout()) {
    setBackground(new Color(168, 76, 162))
    add(welcomeLabel, BorderLayout.NORTH)
    add(buttonPanel, BorderLayout.CENTER)
    setLayout(null)
  }
  new JFrame(title) {
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    setLocation(-10, 0)
    setPreferredSize(new Dimension(Integer.MAX_VALUE, 600))
    add(mainPanel)
    pack()
    setVisible(true)
    setAlwaysOnTop(true)
  }
}

def updateFrame(welcomeLabel: JLabel, buttonPanel: JPanel, title: String) = {
  val mainPanel = new JPanel(new BorderLayout()) {
    setBackground(new Color(168, 76, 162))
    add(welcomeLabel, BorderLayout.NORTH)
    add(buttonPanel, BorderLayout.CENTER)
    setLayout(null)
  }
  val f = getMainFrame(title)
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