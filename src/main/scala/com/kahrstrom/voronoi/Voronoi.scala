package com.kahrstrom.voronoi

import scala.collection.mutable

/*
 * The author of this software is Steven Fortune.  Copyright (c) 1994 by AT&T
 * Bell Laboratories.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */
/*
 * This code was originally written by Stephan Fortune in C code.  I, Shane O'Sullivan,
 * have since modified it, encapsulating it in a C++ class and, fixing memory leaks and
 * adding accessors to the Voronoi Edges.
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */
/*
 * Java Version by Zhenyu Pan
 * Permission to use, copy, modify, and distribute this software for any
 * purpose without fee is hereby granted, provided that this entire notice
 * is included in all copies of any software which is or includes a copy
 * or modification of this software and in all copies of the supporting
 * documentation for such software.
 * THIS SOFTWARE IS BEING PROVIDED "AS IS", WITHOUT ANY EXPRESS OR IMPLIED
 * WARRANTY.  IN PARTICULAR, NEITHER THE AUTHORS NOR AT&T MAKE ANY
 * REPRESENTATION OR WARRANTY OF ANY KIND CONCERNING THE MERCHANTABILITY
 * OF THIS SOFTWARE OR ITS FITNESS FOR ANY PARTICULAR PURPOSE.
 */
/*
 * Note that the sweep line goes from bottom to top.
 */

class Edge(val a: Double, val b: Double, val c: Double, val siteL: Site, val siteR: Site) {
  var endPoints: Array[Site] = new Array[Site](2)

  def discriminant(that: Edge): Double = {
    this.a * that.b - this.b * that.a
  }

  def intersection(that: Edge): Option[Point] = {
    val d = this.discriminant(that)
    if (Math.abs(d) < 1.0e-10) None
    else {
      val xint = (this.c * that.b - that.c * this.b) / d
      val yint = (that.c * this.a - this.c * that.a) / d

      Some(Point(xint, yint))
    }
  }

  def above(p: Point): Boolean = p.x * a + p.y * b > c
  def below(p: Point): Boolean = p.x * a + p.y * b < c

  def hasBothEndpoints: Boolean = endPoints(0) != null && endPoints(1) != null

  override def toString: String = s"Edge($a, $b, $c, $siteL, $siteR)"
}

case class GraphEdge(x1: Double, y1: Double, x2: Double, y2: Double, site1: Int, site2: Int)

class Halfedge(val name: String, val edge: Edge, val pm: Side) {
  // The vertex of the endpoint
  var vertex: Point = null
  // The point where the 'circle event' happens, i.e. the point
  // where he sweep line is when the beach line closes around
  // the vertex.
  var ystar: Point = null

  // This should really be handled by PQHash:
  var PQnext: Halfedge = null

  override def toString: String = s"Halfedge($name, $pm, $edge)"

  def setEndpoint(site: Site, side: Side = pm): Unit = {
    edge.endPoints(side.index) = site
  }

  def leftSite: Site = {
    if (pm == LE) edge.siteL
    else edge.siteR
  }

  def rightSite: Site = {
    if (pm == LE) edge.siteR
    else edge.siteL
  }

  def intersect(that: Halfedge): Option[Point] = {
    def rightOfSite(xint: Double, el: Halfedge): Boolean = {
      val right_of_site = xint >= el.edge.siteR.coord.x
      (right_of_site && el.pm == LE) || (!right_of_site && el.pm == RE)
    }

    if (this.edge == null || that.edge == null) None
    else if (this.edge.siteR == that.edge.siteR) None
    else {
      for {
        p <- this.edge.intersection(that.edge)
        el = if (this.edge.siteR.coord < that.edge.siteR.coord) this else that
        if !rightOfSite(p.x, el)
      } yield p
    }
  }
}

case class Point(x: Double, y: Double) {
  def +(other: Point): Point = {
    Point(x + other.x, y + other.y)
  }

  def -(other: Point): Point = {
    Point(x - other.x, y - other.y)
  }

  def *(that: Point): Double = x * that.x + y * that.y

  def norm2: Double = this * this

  def norm: Double = Math.sqrt(norm2)

  def dist(that: Point): Double = (this - that).norm

  def <(that: Point): Boolean = (this.y < that.y) || (this.y == that.y && this.x < that.x)
  def >(that: Point): Boolean = !(this < that) && !(this == that)
}

case class Site(coord: Point, siteIndex: Int)

sealed trait Side {
  val index: Int
  val inverse: Side
}

object LE extends Side {
  val index: Int = 0
  val inverse: Side = RE
  override def toString: String = "RE"
}

object RE extends Side {
  val index: Int = 1
  val inverse: Side = LE
  override def toString: String = "LE"
}

case class Box(minX: Double, maxX: Double, minY: Double, maxY: Double)

// Priority queue?
class PQHash(sqrt_nsites: Int, boundingBox: Box) {
  private var PQcount: Int = 0
  private var PQmin: Int = 0
  private val PQhashsize: Int = 4 * sqrt_nsites
  private val PQhash: Array[Halfedge] = new Array[Halfedge](PQhashsize)

  private def bucket(he: Halfedge): Int = {
    var bucket: Int = 0
    bucket = ((he.ystar.y - boundingBox.minY) / (boundingBox.maxY - boundingBox.minY) * PQhashsize).toInt
    if (bucket < 0) {
      bucket = 0
    } else if (bucket >= PQhashsize) {
      bucket = PQhashsize - 1
    }
    if (bucket < PQmin) {
      PQmin = bucket
    }
    bucket
  }

  def delete(he: Halfedge) {
    var last: Halfedge = null
    if (he.vertex != null) {
      last = PQhash(bucket(he))
      if (last == he) PQhash(bucket(he)) = last.PQnext
      else {
        while (last.PQnext != he) {
          last = last.PQnext
        }
        last.PQnext = he.PQnext
      }
      PQcount -= 1
      he.vertex = null
      he.PQnext = null
    }
  }

  def insert(he: Halfedge, v: Point, offset: Double) {
    def isAfter(he2: Halfedge): Boolean = {
      he.ystar > he2.ystar
    }

    if (he.PQnext != null) throw new Exception("Inserting he with PQnext!")

    var last: Halfedge = null
    he.vertex = v
    he.ystar = v + Point(0.0, offset)
    last = PQhash(bucket(he))
    if (last == null) {
      PQhash(bucket(he)) = he
    } else if (!isAfter(last)) {
      PQhash(bucket(he)) = he
      he.PQnext = last
    } else {
      var next: Halfedge = null
      while ( {
        next = last.PQnext
        next
      } != null && isAfter(next)) {
        last = next
      }
      he.PQnext = last.PQnext
      last.PQnext = he
    }
    PQcount += 1
  }

  def isEmpty: Boolean = PQcount == 0

  def min: Point = {
    while (PQhash(PQmin) == null) {
      PQmin += 1
    }
    PQhash(PQmin).ystar
  }

  def extractmin: Halfedge = {
    min // Make sure PQmin is up to date
    var curr: Halfedge = null
    curr = PQhash(PQmin)
    PQhash(PQmin) = curr.PQnext
    PQcount -= 1
    curr.PQnext = null
    curr
  }
}

class Voronoi(minDistanceBetweenSites: Double) {
  /** *******************************************************
    * Public methods
    * *******************************************************/

  /**
   *
   * @param xValuesIn Array of X values for each site.
   * @param yValuesIn Array of Y values for each site. Must be identical length to yValuesIn
   * @param minX The minimum X of the bounding box around the voronoi
   * @param maxX The maximum X of the bounding box around the voronoi
   * @param minY The minimum Y of the bounding box around the voronoi
   * @param maxY The maximum Y of the bounding box around the voronoi
   * @return
   */
  def generateVoronoi(xValuesIn: Array[Double], yValuesIn: Array[Double], minX: Double, maxX: Double, minY: Double, maxY: Double): mutable.ListBuffer[GraphEdge] = {
    val count = xValuesIn.length
    val sn: Double = count.toDouble + 4
    val sqrt_nsites = Math.sqrt(sn).toInt
    val allEdges = new mutable.ListBuffer[GraphEdge]
    val sites: Seq[Site] = sortedSites(xValuesIn, yValuesIn)

    val maxBox = Box(min(minX, maxX), max(minX, maxX), min(minY, maxY), max(minY, maxY))
    val boundingBox = Box(xValuesIn.min, xValuesIn.max, yValuesIn.min, yValuesIn.max)

    voronoi_bd(allEdges, sites, sqrt_nsites, xValuesIn.length, maxBox, boundingBox)
    allEdges
  }

  def min(a: Double, b: Double): Double = {
    if (a < b) a
    else b
  }

  def max(a: Double, b: Double): Double = {
    if (a < b) b
    else a
  }

  /** *******************************************************
    * Private methods - implementation details
    * *******************************************************/
  private def sortedSites(xValues: Seq[Double], yValues: Seq[Double]): Seq[Site] = {
    val points: Seq[Point] = (xValues zip yValues) map { case (x, y) => Point(x, y) }
    val sites: Seq[Site] = points.zipWithIndex.map { case (p, i) => Site(p, i) }

    sites.sortWith { (s1, s2) =>
      val p1: Point = s1.coord
      val p2: Point = s2.coord
      p1.y < p2.y || (p1.y == p2.y && p1.x < p2.x)
    }
  }

  // Creates the edge that separates s1 and s2.
  // Normlised so that either a = 1 or b = 1.
  private def separatingLine(s1: Site, s2: Site): Edge = {
    val d = s2.coord - s1.coord
    val adx: Double = Math.abs(d.x)
    val ady: Double = Math.abs(d.y)

    val ta = d.x
    val tb = d.y
    val tc = s1.coord * d + d.norm2 * 0.5
    val (a, b, c) = if (adx > ady) (ta / d.x, tb / d.x, tc / d.x) else (ta / d.y, tb / d.y, tc / d.y)
    new Edge(a, b, c, s1, s2)
  }

  private def clip_line(e: Edge, maxBox: Box): Option[GraphEdge] = {
    if (e.siteL.coord.dist(e.siteR.coord) < minDistanceBetweenSites) return None

    val (s1, s2): (Site, Site) = if (e.a == 1.0 && e.b >= 0.0) (e.endPoints(1), e.endPoints(0)) else (e.endPoints(0), e.endPoints(1))
    var x1: Double = if (s1 == null) -1000 else s1.coord.x
    var x2: Double = if (s2 == null) -1000 else s2.coord.x
    var y1: Double = if (s1 == null) -1000 else s1.coord.y
    var y2: Double = if (s2 == null) -1000 else s2.coord.y

    def clip(v: Double, min: Double, max: Double): Double = {
      if (v < min) min
      else if (v > max) max
      else v
    }

    def needsClipping(v: Double, min: Double, max: Double): Boolean = {
      (v < min) || (v > max)
    }

    if (e.a == 1.0) {
      if (s1 == null || needsClipping(y1, maxBox.minY, maxBox.maxY)) {
        y1 = if (s1 == null) maxBox.minY else clip(s1.coord.y, maxBox.minY, maxBox.maxY)
        x1 = e.c - e.b * y1
      }

      if (s2 == null || needsClipping(y2, maxBox.minY, maxBox.maxY)) {
        y2 = if (s2 == null) maxBox.maxY else clip(s2.coord.y, maxBox.minY, maxBox.maxY)
        x2 = e.c - e.b * y2
      }

      if (((x1 > maxBox.maxX) & (x2 > maxBox.maxX)) || ((x1 < maxBox.minX) & (x2 < maxBox.minX))) {
        return None
      }
      if (x1 > maxBox.maxX) {
        x1 = maxBox.maxX
        y1 = (e.c - x1) / e.b
      } else if (x1 < maxBox.minX) {
        x1 = maxBox.minX
        y1 = (e.c - x1) / e.b
      }
      if (x2 > maxBox.maxX) {
        x2 = maxBox.maxX
        y2 = (e.c - x2) / e.b
      } else if (x2 < maxBox.minX) {
        x2 = maxBox.minX
        y2 = (e.c - x2) / e.b
      }
    } else {
      if (s1 == null || needsClipping(x1, maxBox.minX, maxBox.maxX)) {
        x1 = if (s1 == null) maxBox.minX else clip(s1.coord.x, maxBox.minX, maxBox.maxX)
        y1 = e.c - e.a * x1
      }

      if (s2 == null || needsClipping(x2, maxBox.minX, maxBox.maxX)) {
        x2 = if (s2 == null) maxBox.maxX else clip(s2.coord.x, maxBox.minX, maxBox.maxX)
        y2 = e.c - e.a * x2
      }

      if (((y1 > maxBox.maxY) & (y2 > maxBox.maxY)) || ((y1 < maxBox.minY) & (y2 < maxBox.minY))) {
        return None
      }
      if (y1 > maxBox.maxY) {
        y1 = maxBox.maxY
        x1 = (e.c - y1) / e.a
      } else if (y1 < maxBox.minY) {
        y1 = maxBox.minY
        x1 = (e.c - y1) / e.a
      }
      if (y2 > maxBox.maxY) {
        y2 = maxBox.maxY
        x2 = (e.c - y2) / e.a
      } else if (y2 < maxBox.minY) {
        y2 = maxBox.minY
        x2 = (e.c - y2) / e.a
      }
    }

    Some(new GraphEdge(x1, y1, x2, y2, e.siteL.siteIndex, e.siteR.siteIndex))
  }

  private def clipIfDone(allEdges: mutable.ListBuffer[GraphEdge], edge: Edge, boundingBox: Box): Unit = {
    if (edge.hasBothEndpoints) {
      allEdges ++= clip_line(edge, boundingBox)
    }
  }

  private def voronoi_bd(allEdges: mutable.ListBuffer[GraphEdge], sites: Seq[Site], sqrtNrSites: Int, nrSites: Int, maxBox: Box, boundingBox: Box): Boolean = {
    var nvertices: Int = 0
    var newsite: Site = null
    val pqHash: PQHash = new PQHash(sqrtNrSites, boundingBox)
    val bl: BeachLine = new BeachLine(sqrtNrSites, boundingBox)
    val siteIterator: Iterator[Site] = sites.iterator
    val bottomsite: Site = siteIterator.next()
    newsite = siteIterator.next()
    var keepLooping: Boolean = true
    while (keepLooping) {
      if (newsite != null && (pqHash.isEmpty || (newsite.coord < pqHash.min))) {
        // This is a 'point event':
        val lbnd: Arc = bl.leftbnd(newsite.coord)
        if (lbnd.halfEdge.name == "leftend") {
          val rbnd: Halfedge = lbnd.right.halfEdge
          val site: Site = bottomsite
          val e: Edge = separatingLine(site, newsite)
          val bisector: Arc = new Arc(new Halfedge("leftbi", e, LE))
          lbnd.insert(bisector)
          val bisector2: Arc = new Arc(new Halfedge("rightbi", e, RE))
          bisector.insert(bisector2)
          bisector2.halfEdge.intersect(rbnd).foreach { p =>
            pqHash.insert(bisector2.halfEdge, p, p.dist(newsite.coord))
          }
        } else {
          val rbnd: Halfedge = lbnd.right.halfEdge
          val site: Site = lbnd.halfEdge.rightSite
          val e: Edge = separatingLine(site, newsite)
          val bisector: Arc = new Arc(new Halfedge("leftbi", e, LE))
          lbnd.insert(bisector)
          pqHash.delete(lbnd.halfEdge)
          lbnd.halfEdge.intersect(bisector.halfEdge).foreach { p =>
            pqHash.insert(lbnd.halfEdge, p, p.dist(newsite.coord))
          }
          val bisector2 = new Arc(new Halfedge("rightbi", e, RE))
          bisector.insert(bisector2)
          bisector2.halfEdge.intersect(rbnd).foreach { p =>
            pqHash.insert(bisector2.halfEdge, p, p.dist(newsite.coord))
          }
        }
        if (siteIterator.hasNext) newsite = siteIterator.next()
        else newsite = null
      } else if (!pqHash.isEmpty) {
        // This is a 'circle/vertex event'
        val lbnd: Arc = bl.find(pqHash.extractmin)
        val llbnd: Arc = lbnd.left
        val rbnd: Arc = lbnd.right
        val rrbnd: Arc = rbnd.right
        val v: Site = Site(lbnd.halfEdge.vertex, nvertices)
        nvertices += 1

        lbnd.halfEdge.setEndpoint(v)
        clipIfDone(allEdges, lbnd.halfEdge.edge, maxBox)
        rbnd.halfEdge.setEndpoint(v)
        clipIfDone(allEdges, rbnd.halfEdge.edge, maxBox)
        lbnd.delete()
        pqHash.delete(rbnd.halfEdge)
        rbnd.delete()

        val (bot: Site, top: Site, pm: Side) = {
          val a: Site = lbnd.halfEdge.leftSite
          val b: Site = rbnd.halfEdge.rightSite
          if (a.coord.y > b.coord.y) (b, a, RE)
          else (a, b, LE)
        }
        val e: Edge = separatingLine(bot, top)
        val bisector = new Arc(new Halfedge("midbi", e, pm))
        llbnd.insert(bisector)
        bisector.halfEdge.setEndpoint(v, pm.inverse)
        clipIfDone(allEdges, e, maxBox)
        pqHash.delete(llbnd.halfEdge)
        llbnd.halfEdge.intersect(bisector.halfEdge).foreach { p =>
          pqHash.insert(llbnd.halfEdge, p, p.dist(bot.coord))
        }
        bisector.halfEdge.intersect(rrbnd.halfEdge).foreach { p =>
          pqHash.insert(bisector.halfEdge, p, p.dist(bot.coord))
        }
      } else {
        keepLooping = false
      }
    }
    // Add all remaining edges in the beach line
    var lbnd = bl.ELleftend.right
    while (lbnd != bl.ELrightend) {
      allEdges ++= clip_line(lbnd.halfEdge.edge, maxBox)
      lbnd = lbnd.right
    }
    true
  }
}