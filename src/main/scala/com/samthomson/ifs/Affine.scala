/*
 * Copyright 2012 Sam Thomson <sam@samthomson.com>
 */
package com.samthomson.ifs

import java.awt.{Color, Graphics2D, Polygon, Shape, BasicStroke, AlphaComposite}
import java.awt.geom.{AffineTransform, Rectangle2D, GeneralPath, Point2D}

/**
 * Makes working with `Point2D`s as easy as working with tuples
 */
case class Point(x: Double, y: Double) {
  def +(other: Point) = Point(x + other.x, y + other.y)
  def -(other: Point) = Point(x - other.x, y - other.y)
}

object Point {
  implicit def point2dToPoint(p: Point2D): Point = Point(p.getX(), p.getY())
  implicit def pointToPoint2d(p: Point): Point2D = new Point2D.Double(p.x, p.y)
  implicit def tupleToPoint[T <% Double, U <% Double](p: (T, U)): Point = {
    Point(p._1, p._2)
  }
  implicit def pointToTuple(p: Point): (Double, Double) = (p.x, p.y)
}


/**
 * Something you can grab onto and move a vertex of an affine
 */
abstract class Handle(parent: Affine, center: Point, color: Color) {
    val size = .025;
    var _center = center
    val _parent = parent

    /** The filled-in square with which we represent this Handle */
    def rect = new Rectangle2D.Double(_center.x - size,  _center.y - size,
                                      size * 2,          size * 2)

    def contains(p: Point): Option[Handle] = {
      if(rect.contains(p)) Some(this) else None
    }

    /** Hook so that each handle can manipulate its affine in a different way */
    def moveToHelper(p: Point): Unit;

    /**
     * Moves this handle to given location. Calls the `moveToHelper` callback
     */
    def moveTo(p: Point) = {
      _center = p
      moveToHelper(p)
    }

    def paint(g: Graphics2D) {
        g.setPaint(color)
        g.fill(rect)
    }
}

/**
 * Wrapper around AffineTransform, with methods for display and interaction
 */
class Affine(t: AffineTransform) {
  /** Convenient constructor */
  def this(i_x: Double, i_y: Double,
           j_x: Double, j_y: Double,
           t_x: Double, t_y: Double) = {
    this(new AffineTransform(i_x, i_y,
                             j_x, j_y,
                             t_x, t_y))
  }
  /* The source of truth for this transform */
  private var _t = t

  /** Views on the locations of the three vertices */
  def origin = this.times(0, 0)
  def iHat = this.times(1, 0)
  def jHat = this.times(0, 1)

  /**
   * Handles for our three vertices.
   * TODO: These get recreated every time they're accessed, which is maybe
   * inefficient
   */
  def originHandle = new Handle(this, origin, Color.BLUE) {
    // Moves just the blue handle
    //def moveToHelper(p: Point) = setTransform(iHat, jHat, p)

    /* Moves the whole affine */
    def moveToHelper(p: Point) = {
      val delta = p - origin
      setTransform(iHat + delta, jHat + delta, p)
    }
  }
  def iHandle = new Handle(this, iHat, Color.RED) {
    def moveToHelper(p: Point) = setTransform(p, jHat, origin)
  }
  def jHandle = new Handle(this, jHat, Color.GREEN) {
    def moveToHelper(p: Point) = setTransform(iHat, p, origin)
  }

  /** Finds which (if any) handle contains the given point (in user space) */
  def findHandle(p: Point): Option[Handle] = {
      originHandle.contains(p) orElse
          iHandle.contains(p) orElse
          jHandle.contains(p)
  }

  private val aff = this
  /** the filled in shape with which we represent this affine */
  def parallelogram = new Rectangle2D.Double(0, 0, 1, 1) {
    def paint(g: Graphics2D) = {
      val oldTransform = g.getTransform()
      val oldComposite = g.getComposite()
      g.transform(aff)
      g.setComposite(AlphaComposite.getInstance(AlphaComposite.SRC_OVER, .5f))
      g.setColor(Color.lightGray)
      g.fill(this)
      // put `g` back how we found it
      g.setComposite(oldComposite)
      g.setTransform(oldTransform)
    }
  }

  /** Applies this transform to a 2D point */
  def times(point: Point): Point = _t.transform(point, null)

  /**
   * Sets this affine by specifying the locations of each of the three vertices
   */
  def setTransform(i: Point, j: Point, t: Point) = {
    _t.setTransform(i.x - t.x, i.y - t.y,
                    j.x - t.x, j.y - t.y,
                    t.x,       t.y)
  }

  /** Paints the filled-in parallelogram and the three handles */
  def paint(g: Graphics2D) = {
    parallelogram.paint(g)
    originHandle.paint(g)
    iHandle.paint(g)
    jHandle.paint(g)
  }
}

object Affine {
  implicit def transformToAffine(t: AffineTransform): Affine = new Affine(t)
  implicit def affineToTransform(a: Affine): AffineTransform = a._t
}
