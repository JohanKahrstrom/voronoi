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

  override def toString(): String = s"Edge($a, $b, $c, $siteL, $siteR)"
}

case class GraphEdge(val x1: Double, val y1: Double, val x2: Double, val y2: Double, val site1: Int, val site2: Int)

class PQLink(val ELpm: Side) {

}

class Halfedge(val name: String, val ELedge: Edge, val ELpm: Side) {
  var deleted: Boolean = false
  var vertex: Point = null
  var ystar: Double = .0
  var PQnext: Halfedge = null

  override def toString(): String = s"Halfedge($name, $ELpm, $ELedge)"

  def setEndpoint(site: Site, side: Side = ELpm): Unit = {
    ELedge.endPoints(side.index) = site
  }

  def leftSite: Site = {
    if (ELpm == LE) ELedge.siteL
    else ELedge.siteR
  }

  def rightSite: Site = {
    if (ELpm == LE) ELedge.siteR
    else ELedge.siteL
  }

  def intersect(that: Halfedge): Option[Point] = {
    def rightOfSite(xint: Double, el: Halfedge): Boolean = {
      val right_of_site = xint >= el.ELedge.siteR.coord.x
      (right_of_site && el.ELpm == LE) || (!right_of_site && el.ELpm == RE)
    }

    if (this.ELedge == null || that.ELedge == null) None
    else if (this.ELedge.siteR == that.ELedge.siteR) None
    else {
      for {
        p <- this.ELedge.intersection(that.ELedge)
        el = if (this.ELedge.siteR.coord < that.ELedge.siteR.coord) this else that
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
}

case class Site(coord: Point, siteIndex: Int)

sealed trait Side {
  val index: Int
  val inverse: Side
}

object LE extends Side {
  val index: Int = 0
  val inverse: Side = RE
  override def toString(): String = "RE"
}

object RE extends Side {
  val index: Int = 1
  val inverse: Side = LE
  override def toString(): String = "LE"
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
    bucket = ((he.ystar - boundingBox.minY) / (boundingBox.maxY - boundingBox.minY) * PQhashsize).toInt
    if (bucket < 0) {
      bucket = 0
    }
    if (bucket >= PQhashsize) {
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
      he.ystar > he2.ystar || (he.ystar == he2.ystar && v.x > he2.vertex.x)
    }

    if (he.PQnext != null) throw new Exception("Inserting he with PQnext!")

    var last: Halfedge = null
    var next: Halfedge = null
    he.vertex = v
    he.ystar = v.y + offset
    last = PQhash(bucket(he))
    if (last == null) {
      PQhash(bucket(he)) = he
    } else if (!isAfter(last)) {
      PQhash(bucket(he)) = he
      he.PQnext = last
    } else {
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
    Point(PQhash(PQmin).vertex.x, PQhash(PQmin).ystar)
  }

  def extractmin: Halfedge = {
    var curr: Halfedge = null
    curr = PQhash(PQmin)
    PQhash(PQmin) = curr.PQnext
    PQcount -= 1
    curr.PQnext = null
    curr
  }
}

class ELNode(val halfEdge: Halfedge) {
  var left: ELNode = _
  var right: ELNode = _

  def insert(newHe: ELNode) {
    newHe.left = this
    newHe.right = this.right
    this.right.left = newHe
    this.right = newHe
  }

  def delete(): Unit = {
    this.left.right = this.right
    this.right.left = left
    halfEdge.deleted = true
  }
}

class ELt(sqrt_nsites: Int, boundingBox: Box) {
  private val ELhashsize: Int = 2 * sqrt_nsites
  private val ELhash: Array[ELNode] = new Array[ELNode](ELhashsize)
  val ELleftend: ELNode = new ELNode(new Halfedge("leftend", null, LE))
  val ELrightend: ELNode = new ELNode(new Halfedge("rightend", null, LE))
  ELleftend.left = null
  ELleftend.right = ELrightend
  ELrightend.left = ELleftend
  ELrightend.right = null
  ELhash(0) = ELleftend
  ELhash(ELhashsize - 1) = ELrightend

  // TODO: Very inefficient, optimise
  def find(he: Halfedge): ELNode = {
    var bucket = 0
    while (bucket < ELhash.size) {
      var node = ELhash(bucket)
      while (node != null) {
        if (node.halfEdge == he) return node
        else node = node.right
      }
      bucket += 1
    }
    null
  }

  private def get(b: Int): ELNode = {
    val he: ELNode = ELhash(b)
    if (he == null || !he.halfEdge.deleted) he
    else {
      ELhash(b) = null
      null
    }
  }

  private def getBucket(p: Point): Int = {
    val bucket = ((p.x - boundingBox.minY) / (boundingBox.maxX - boundingBox.minX) * ELhashsize).toInt
    if (bucket < 0) 0
    else if (bucket >= ELhashsize) ELhashsize - 1
    else bucket
  }

  def getHalfedge(bucket: Int): ELNode = {
    var he = get(bucket)
    var i: Int = 0
    if (he == null) {
      i = 1
      while (i < ELhashsize && he == null) {
        if (get(bucket - i) != null) he = get(bucket - i)
        else if (get(bucket + i) != null) he = get(bucket + i)
        i += 1
      }
    }
    he
  }

  def leftbnd(p: Point): ELNode = {
    val bucket: Int = getBucket(p)
    var he: ELNode = getHalfedge(bucket)
    if (he == ELleftend || (he != ELrightend && right_of(he.halfEdge, p))) {
      do {
        he = he.right
      } while (he != ELrightend && right_of(he.halfEdge, p))
      he = he.left
    } else {
      do {
        he = he.left
      } while (he != ELleftend && !right_of(he.halfEdge, p))
    }
    if (bucket > 0 && bucket < ELhashsize - 1) {
      ELhash(bucket) = he
    }
    he
  }

  private def right_of(el: Halfedge, p: Point): Boolean = {
    def above(e: Edge, right_of_site: Boolean): Boolean = {
      if (e.a == 1.0) {
        val dp = p - e.siteR.coord
        val ds = e.siteR.coord - e.siteL.coord
        val a = e.b * (dp.x * dp.x - dp.y * dp.y) < ds.x * dp.y * (1.0 + 2.0 * dp.x / ds.x + e.b * e.b)
        val temp = if (e.b < 0.0) !a else a

        if ((!right_of_site & (e.b < 0.0)) | (right_of_site & (e.b >= 0.0))) {
          val aa = e.a * dp.y < e.b * dp.x
          if (aa) temp
          else true
        } else {
          val aa = if (e.b < 0.0) !e.above(p) else e.above(p)
          if (aa) temp
          else false
        }
      } else {
        val yl = e.c - e.a * p.x
        val t1 = - yl + e.b * p.y
        val t2 = p.x - e.siteR.coord.x
        val t3 = yl - e.siteR.coord.y
        t1 * t1 > t2 * t2 + t3 * t3
      }
    }

    val e: Edge = el.ELedge
    val right_of_site: Boolean = p.x > e.siteR.coord.x
    if (right_of_site && el.ELpm == LE) true
    else if (!right_of_site && el.ELpm == RE) false
    else if (el.ELpm == LE) above(e, right_of_site)
    else !above(e, right_of_site)
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
    val sites: Seq[Site] = (points zipWithIndex) map { case (p, i) => Site(p, i) }

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
    var newintstar: Point = null
    val pqHash: PQHash = new PQHash(sqrtNrSites, boundingBox)
    val el: ELt = new ELt(sqrtNrSites, boundingBox)
    val siteIterator: Iterator[Site] = sites.iterator
    val bottomsite: Site = siteIterator.next()
    newsite = siteIterator.next()
    var keepLooping: Boolean = true
    while (keepLooping) {
      if (!pqHash.isEmpty) {
        newintstar = pqHash.min
      }

      if (newsite != null && (pqHash.isEmpty || (newsite.coord < newintstar))) {
        // p is a site in *(V):
        val lbnd: ELNode = el.leftbnd(newsite.coord)
        if (lbnd.halfEdge.name == "leftend") {
          val rbnd: Halfedge = lbnd.right.halfEdge
          val site: Site = bottomsite
          val e: Edge = separatingLine(site, newsite)
          val bisector: ELNode = new ELNode(new Halfedge("leftbi", e, LE))
          lbnd.insert(bisector)
          val bisector2: ELNode = new ELNode(new Halfedge("rightbi", e, RE))
          bisector.insert(bisector2)
          bisector2.halfEdge.intersect(rbnd).foreach { p =>
            pqHash.insert(bisector2.halfEdge, p, p.dist(newsite.coord))
          }
        } else {
          val rbnd: Halfedge = lbnd.right.halfEdge
          val site: Site = lbnd.halfEdge.rightSite
          val e: Edge = separatingLine(site, newsite)
          val bisector: ELNode = new ELNode(new Halfedge("leftbi", e, LE))
          lbnd.insert(bisector)
          pqHash.delete(lbnd.halfEdge)
          lbnd.halfEdge.intersect(bisector.halfEdge).foreach { p =>
            pqHash.insert(lbnd.halfEdge, p, p.dist(newsite.coord))
          }
          val bisector2 = new ELNode(new Halfedge("rightbi", e, RE))
          bisector.insert(bisector2)
          bisector2.halfEdge.intersect(rbnd).foreach { p =>
            pqHash.insert(bisector2.halfEdge, p, p.dist(newsite.coord))
          }
        }
        if (siteIterator.hasNext) newsite = siteIterator.next()
        else newsite = null
      } else if (!pqHash.isEmpty) {
        val lbndE: Halfedge = pqHash.extractmin
        val lbnd: ELNode = el.find(lbndE)
        val llbnd: ELNode = lbnd.left
        val rbnd: ELNode = lbnd.right
        val rrbnd: ELNode = rbnd.right
        val v: Site = Site(lbnd.halfEdge.vertex, nvertices)
        nvertices += 1

        lbnd.halfEdge.setEndpoint(v)
        clipIfDone(allEdges, lbnd.halfEdge.ELedge, maxBox)
        rbnd.halfEdge.setEndpoint(v)
        clipIfDone(allEdges, rbnd.halfEdge.ELedge, maxBox)
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
        val bisector = new ELNode(new Halfedge("midbi", e, pm))
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
    var lbnd = el.ELleftend.right
    while (lbnd != el.ELrightend) {
      allEdges ++= clip_line(lbnd.halfEdge.ELedge, maxBox)
      lbnd = lbnd.right
    }
    true
  }
}