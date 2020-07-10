package client.ui

import java.awt._

import javax.swing.UIDefaults

import scala.swing.Label

object ViewConstants {
  var defFont = new Font("Arial", 0, 16)
  val nearWhite=new Color(254,254,254)
  var defaultRowHeight: Int = 25
  var tableFont = new Font("Arial", 0, 14)
  var smallFont = new Font("Arial", 0, 12)
  var tinyFont = new Font("Arial", 0, 10)
  var tableTypeFont = new Font("Arial", Font.ITALIC, 11)
  var labelFont = new Font("Arial", 0, 14)
  var questionFont = new Font("Arial", 0, 15)
  var errorFont = new Font("Arial", 1, 14)
  val buttonBackgroundColor = new Color(214, 217, 223)
  var lineCatchDistance = 6
  var pointCatchDistance = 8
  var dragTreshold = 8
  var fontScale = 100
  var polyLineTo= 1
  var showToast=1
  var showHitPoints=1
  var antialias=1
  var stopFX=0
  var imagePath=""
  var selectBorderWidth=1
  var backgroundLayerTrans=40

  lazy val layerComposite: AlphaComposite =AlphaComposite.getInstance(AlphaComposite.SRC_OVER,backgroundLayerTrans.toFloat/100f)
  //var hatchLineWidth=4

  def label(text: String = ""): Label = {
    val res = new Label(text)
    res.font = labelFont
    res
  }

  val hoverColor: Color = Color.cyan.darker

  val leftPanelColor = new Color(247, 247, 252)
  val eitherColor = new Color(225, 225, 225)
  lazy val sidePanelWidth: Int = 195 * ViewConstants.fontScale / 100

  lazy val buttonDefaults: UIDefaults = {
    val res = new UIDefaults()
    res.put("Button.contentMargins", new Insets(6, 6, 6, 6))
    res
  }

  lazy val toggleButtonDefaults: UIDefaults = {
    val res = new UIDefaults()
    res.put("ToggleButton.contentMargins", new Insets(7, 7, 7, 7))
    res
  }
  lazy val buttonSize = new Dimension(sidePanelWidth - 10, 30)
  lazy val minButtonSize = new Dimension(sidePanelWidth - 10 * ViewConstants.fontScale / 100, 40)

  lazy val labelSize=new Dimension(ViewConstants.sidePanelWidth - 30, (new Label).preferredSize.height)

}
