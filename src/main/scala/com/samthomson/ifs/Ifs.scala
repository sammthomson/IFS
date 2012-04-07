/*
 * Copyright 2012 Sam Thomson <sam@samthomson.com>
 */
package com.samthomson.ifs

import java.awt.{Graphics2D, BasicStroke, Color}
import java.awt.geom.Rectangle2D
import scala.util.Random


/*
 * A collection of Affines, with methods for displaying and editing
 */
class IFS() {
    private val warmupSteps = 100
    private val iterations = 30000
    private val random = new Random()

    private var affines: List[Affine] = List()

    def addAffine(a: Affine) = {
      affines ::= a
      a
    }

    def deleteAffine(a: Affine) = affines = affines.remove(x => x == a)

    /** Finds which handle (if any) contains the given point */
    def findHandle(p: Point): Option[Handle] = {
      var handle: Option[Handle] = None
      affines.find(a => {
        handle = a.findHandle(p)
        !handle.isEmpty
      })
      handle
    }

    /** Paints the affines on editor panel */
    def paintEditor(g: Graphics2D) = affines.foreach(_.paint(g))

    private def pickAffine = affines(random.nextInt(affines.length))

    private def takeStep(p: Point) = pickAffine.times(p)

    /** Paints the resultant fractal on the preview panel */
    def paintPreview(g: Graphics2D) = {
      // half a pixel
      val size = .5 / Math.abs(g.getTransform().getScaleX())
      g.setStroke(new BasicStroke(0f))
      g.setColor(Color.darkGray)
      // warmup before you start rendering
      var p = Point(.5, .5)
      for(i <- 1 to warmupSteps) p = takeStep(p)
      // real thing
      for(i <- 1 to iterations) {
        p = takeStep(p)
        g.draw(new Rectangle2D.Double(p.x, p.y,
                                      size, size))
      }
    }
}
