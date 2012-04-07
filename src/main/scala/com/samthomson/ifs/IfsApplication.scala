/*
 * Copyright 2012 Sam Thomson <sam@samthomson.com>
 */
package com.samthomson.ifs

import java.awt.{BasicStroke, Color, Graphics2D, RenderingHints}
import java.awt.geom.Rectangle2D
import scala.swing.{MainFrame, Orientation, Panel, SimpleSwingApplication,
    SplitPane, BoxPanel, Button}
import scala.swing.event.{KeyTyped, MouseDragged, MousePressed, MouseReleased,
    ButtonClicked}
import scala.swing.Swing.pair2Dimension
import com.samthomson.ifs.Point.point2dToPoint


/* Configs for converting from user space to screen space */
object Screen {
  val width = 600
  val height = 600
  // how big should the unit square be
  val scale = Math.min(width, height) * .5
  // position the unit square in the middle of the screen
  val transform = new Affine(scale,        0,
                             0,            - scale,
                             width * .25,  height * .75)
  val inverse: Affine = transform.createInverse()
}


object IfsApplication extends SimpleSwingApplication {
  // initialize an IFS with the Seirpinski gasket
  val ifs = new IFS()
  ifs.addAffine(new Affine(.5, 0,
                           0,  .5,
                           0,  0))
  ifs.addAffine(new Affine(.5, 0,
                           0,  .5,
                           .5, 0))
  ifs.addAffine(new Affine(.5,   0,
                           0,   .5,
                           .25, .5))

  /* the currently selected handle*/
  var currentHandle: Option[Handle] = None
  /* the last selected affine */
  var currentAffine: Option[Affine] = None

  val addButton = new Button("Add (a)")
  val deleteButton = new Button("Delete (d)")

  /**
   * Holds common functionality between the editing panel and the preview panel
   */
  abstract class DisplayPanel extends Panel {
    background = Color.white
    focusable = true

    // Rendering specific to the implementing subclass
    def paintHelper(g: Graphics2D): Unit;

    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      g.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
                         RenderingHints.VALUE_ANTIALIAS_ON)
      // So we can work in user space
      g.transform(Screen.transform)
      // Draw an outline of the unit square
      g.setColor(Color.lightGray)
      g.setStroke(new BasicStroke(0f))
      g.draw(new Rectangle2D.Double(0, 0,
                                    1, 1))
      // Rendering specific to the implementing subclass
      paintHelper(g)
    }

  }

  /* The editing panel */
  val editor = new DisplayPanel {
    def paintHelper(g: Graphics2D): Unit = ifs.paintEditor(g)
    /*
     * Event Handling
     */
    listenTo(mouse.clicks, mouse.moves, keys, addButton, deleteButton)
    reactions += {
      case e: MousePressed  =>
        currentHandle = ifs.findHandle(Screen.inverse.times(e.point))
        currentHandle match {
          case Some(h) => currentAffine = Some(h._parent)
          case _ => currentAffine = None
        }
      case e: MouseDragged  =>
        currentHandle map {_.moveTo(Screen.inverse.times(e.point))}
        repaintAll()
      case e: MouseReleased =>
        currentHandle = None
      case KeyTyped(_, 'a', _, _) | ButtonClicked(`addButton`) =>
        // add a new affine and make it current
        currentAffine = Some(ifs.addAffine(new Affine(.5, 0,
                                                      0,  .5,
                                                      0,  0)))
        repaintAll()
      case KeyTyped(_, 'd', _, _) | ButtonClicked(`deleteButton`) =>
        // delete the current affine
        currentAffine map {ifs.deleteAffine(_)}
        repaintAll()
    }
  }

  /* The preview panel */
  val preview = new DisplayPanel {
    def paintHelper(g: Graphics2D) = ifs.paintPreview(g)
  }

  /* Repaint both panels */
  def repaintAll() {
    editor.repaint()
    preview.repaint()
  }

  /* The main window */
  def top = new MainFrame {
    preferredSize = (Screen.width * 2, Screen.height)
    title = "IFS"

    contents = new SplitPane(Orientation.Vertical) {
      leftComponent = new BoxPanel(Orientation.Vertical) {
        contents += editor
        contents += new BoxPanel(Orientation.Horizontal) {
          contents += addButton
          contents += deleteButton
        }
      }
      rightComponent = preview
      resizeWeight = .5
    }
  }
}
