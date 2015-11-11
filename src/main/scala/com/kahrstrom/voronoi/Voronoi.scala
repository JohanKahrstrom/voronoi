package com.kahrstrom.voronoi

import scala.annotation.tailrec
import scala.util.Random

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

class Edge(val a: Double, val b: Double, val c: Double, val regL: Site, val regR: Site) {
  var endPoints: Array[Site] = new Array[Site](2)

  def discriminant(that: Edge): Double = {
    this.a * that.b - this.b * that.a
  }

  def above(p: Point): Boolean = p.x * a + p.y * b > c
  def below(p: Point): Boolean = p.x * a + p.y * b < c
}

case class GraphEdge(val x1: Double, val y1: Double, val x2: Double, val y2: Double, val site1: Int, val site2: Int)

class PQLink(val ELpm: Side) {

}

class Halfedge(val ELedge: Edge, val ELpm: Side) {
  var ELleft: Halfedge = null
  var ELright: Halfedge = null
  var deleted: Boolean = false
  var vertex: Point = null
  var ystar: Double = .0
  var PQnext: Halfedge = null

  def insert(newHe: Halfedge) {
    newHe.ELleft = this
    newHe.ELright = this.ELright
    this.ELright.ELleft = newHe
    this.ELright = newHe
  }

  def delete(): Unit = {
    ELleft.ELright = ELright
    ELright.ELleft = ELleft
    deleted = true
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
}

case class Site(coord: Point, siteIndex: Int)

sealed trait Side {
  val index: Int
  val inverse: Side
}

object LE extends Side {
  val index: Int = 0
  val inverse: Side = RE
}

object RE extends Side {
  val index: Int = 1
  val inverse: Side = LE
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
    }
    else if (!isAfter(last)) {
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

class ELt(sqrt_nsites: Int, boundingBox: Box) {
  private val ELhashsize: Int = 2 * sqrt_nsites
  private val ELhash: Array[Halfedge] = new Array[Halfedge](ELhashsize)
  var ELleftend: Halfedge = null
  var ELrightend: Halfedge = null

  ELleftend = new Halfedge(null, LE)
  ELrightend = new Halfedge(null, LE)
  ELleftend.ELleft = null
  ELleftend.ELright = ELrightend
  ELrightend.ELleft = ELleftend
  ELrightend.ELright = null
  ELhash(0) = ELleftend
  ELhash(ELhashsize - 1) = ELrightend

  private def get(b: Int): Halfedge = {
    val he: Halfedge = ELhash(b)
    if (he == null || !he.deleted) he
    else {
      ELhash(b) = null
      null
    }
  }

  def getBucket(p: Point): Int = {
    val bucket = ((p.x - boundingBox.minY) / (boundingBox.maxX - boundingBox.minX) * ELhashsize).toInt
    if (bucket < 0) 0
    else if (bucket >= ELhashsize) ELhashsize - 1
    else bucket
  }

  def leftbnd(p: Point): Halfedge = {
    var i: Int = 0
    val bucket: Int = getBucket(p)
    var he: Halfedge = null
    he = get(bucket)
    if (he == null) {
      i = 1
      while (i < ELhashsize && he == null) {
        if (get(bucket - i) != null) he = get(bucket - i)
        else if (get(bucket + i) != null) he = get(bucket + i)
        i += 1
      }
    }
    if (he == ELleftend || (he != ELrightend && right_of(he, p))) {
      do {
        he = he.ELright
      } while (he != ELrightend && right_of(he, p))
      he = he.ELleft
    } else {
      do {
        he = he.ELleft
      } while (he != ELleftend && !right_of(he, p))
    }
    if (bucket > 0 && bucket < ELhashsize - 1) {
      ELhash(bucket) = he
    }
    he
  }

  private def right_of(el: Halfedge, p: Point): Boolean = {
    def above(e: Edge, topsite: Site, right_of_site: Boolean): Boolean = {
      if (e.a == 1.0) {
        val dyp = p.y - topsite.coord.y
        val dxp = p.x - topsite.coord.x
        if ((!right_of_site & (e.b < 0.0)) | (right_of_site & (e.b >= 0.0))) {
          val aa = dyp >= e.b * dxp
          if (!aa) {
            val dxs = topsite.coord.x - e.regL.coord.x
            val a = e.b * (dxp * dxp - dyp * dyp) < dxs * dyp * (1.0 + 2.0 * dxp / dxs + e.b * e.b)
            if (e.b < 0.0) !a
            else a
          } else aa
        } else {
          var a = e.above(p)
          if (e.b < 0.0) a = !a
          if (a) {
            val dxs = topsite.coord.x - e.regL.coord.x
            val a = e.b * (dxp * dxp - dyp * dyp) < dxs * dyp * (1.0 + 2.0 * dxp / dxs + e.b * e.b)
            if (e.b < 0.0) !a
            else a
          } else a
        }
      } else {
        val yl = e.c - e.a * p.x
        val t1 = - yl + e.b * p.y
        val t2 = p.x - topsite.coord.x
        val t3 = yl - topsite.coord.y
        t1 * t1 > t2 * t2 + t3 * t3
      }
    }

    val e: Edge = el.ELedge
    val topsite: Site = e.regR
    val right_of_site: Boolean = p.x > topsite.coord.x
    if (right_of_site && el.ELpm == LE) true
    else if (!right_of_site && el.ELpm == RE) false
    else if (el.ELpm == LE) above(e, topsite, right_of_site)
    else !above(e, topsite, right_of_site)
  }
}

class Voronoi(minDistanceBetweenSites: Double) {
  private var siteidx: Int = 0
  private var sites: Array[Site] = null
  private var allEdges: java.util.List[GraphEdge] = null

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
  def generateVoronoi(xValuesIn: Array[Double], yValuesIn: Array[Double], minX: Double, maxX: Double, minY: Double, maxY: Double): java.util.List[GraphEdge] = {
    val count = xValuesIn.length
    val sn: Double = count.toDouble + 4
    val sqrt_nsites = Math.sqrt(sn).toInt

    sort(xValuesIn, yValuesIn, count)

    val maxBox = Box(min(minX, maxX), max(minX, maxX), min(minY, maxY), max(minY, maxY))
    val boundingBox = Box(xValuesIn.min, xValuesIn.max, yValuesIn.min, yValuesIn.max)

    siteidx = 0
    voronoi_bd(sites, sqrt_nsites, xValuesIn.length, maxBox, boundingBox)
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
  private def sort(xValuesIn: Array[Double], yValuesIn: Array[Double], count: Int) {
    sites = null
    allEdges = new java.util.LinkedList[GraphEdge]
    val xValues: Array[Double] = new Array[Double](count)
    val yValues: Array[Double] = new Array[Double](count)

    var i: Int = 0
    while (i < count) {
      xValues(i) = xValuesIn(i)
      yValues(i) = yValuesIn(i)
      i += 1
    }

    sortNode(xValues, yValues, count)
  }

  private def sortNode(xValues: Array[Double], yValues: Array[Double], numPoints: Int) {
    var i: Int = 0
    sites = new Array[Site](numPoints)
    i = 0
    while (i < numPoints) {
      {
        sites(i) = Site(Point(xValues(i), yValues(i)), i)
      }
      i += 1
    }
    qsort(sites)
  }

  private def qsort(sites: Array[Site]) {
    val listSites: java.util.List[Site] = new java.util.ArrayList[Site](sites.length)
    for (s <- sites) {
      listSites.add(s)
    }
    java.util.Collections.sort(listSites, new java.util.Comparator[Site] {
      def compare(p1: Site, p2: Site): Int = {
        val s1: Point = p1.coord
        val s2: Point = p2.coord
        if (s1.y < s2.y) -1
        else if (s1.y > s2.y) 1
        else if (s1.x < s2.x) -1
        else if (s1.x > s2.x) 1
        else 0
      }
    })

    var i: Int = 0
    while (i < sites.length) {
      sites(i) = listSites.get(i)
      i += 1
    }
  }

  private def bisect(s1: Site, s2: Site): Edge = {
    val d = s2.coord - s1.coord
    val adx: Double = Math.abs(d.x)
    val ady: Double = Math.abs(d.y)

    val ta = d.x
    val tb = d.y
    val tc = s1.coord * d + d.norm2 * 0.5
    val (a, b, c) = if (adx > ady) (ta / d.x, tb / d.x, tc / d.x) else (ta / d.y, tb / d.y, tc / d.y)
    new Edge(a, b, c, s1, s2)
  }

  private def leftreg(he: Halfedge): Site = {
    if (he.ELpm == LE) he.ELedge.regL
    else he.ELedge.regR
  }

  private def rightreg(he: Halfedge, bottomsite: Site): Site = {
    if (he.ELedge == null) bottomsite
    else if (he.ELpm == LE) he.ELedge.regR
    else he.ELedge.regL
  }

  def norm2(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
  }

  def norm(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
    Math.sqrt(norm2(x1, y1, x2, y2))
  }

  private def clip_line(e: Edge, maxBox: Box): Option[GraphEdge] = {
    val (s1, s2): (Site, Site) = if (e.a == 1.0 && e.b >= 0.0) (e.endPoints(1), e.endPoints(0)) else (e.endPoints(0), e.endPoints(1))
    var x1: Double = e.regL.coord.x
    var x2: Double = e.regR.coord.x
    var y1: Double = e.regL.coord.y
    var y2: Double = e.regR.coord.y
    if (norm(x1, y1, x2, y2) < minDistanceBetweenSites) {
      return None
    }
    if (e.a == 1.0) {
      y1 = if (s1 != null && s1.coord.y > maxBox.minY && s1.coord.y <= maxBox.maxY) {
        s1.coord.y
      } else if (s1 != null && s1.coord.y > maxBox.maxY) {
        maxBox.maxY
      } else {
        maxBox.minY
      }
      x1 = e.c - e.b * y1
      y2 = if (s2 != null && s2.coord.y >= maxBox.minY && s2.coord.y < maxBox.maxY) {
        s2.coord.y
      } else if (s2 != null && s2.coord.y < maxBox.minY) {
        maxBox.minY
      } else {
        maxBox.maxY
      }
      x2 = e.c - e.b * y2
      if (((x1 > maxBox.maxX) & (x2 > maxBox.maxX)) | ((x1 < maxBox.minX) & (x2 < maxBox.minX))) {
        return None
      }
      if (x1 > maxBox.maxX) {
        x1 = maxBox.maxX
        y1 = (e.c - x1) / e.b
      }
      if (x1 < maxBox.minX) {
        x1 = maxBox.minX
        y1 = (e.c - x1) / e.b
      }
      if (x2 > maxBox.maxX) {
        x2 = maxBox.maxX
        y2 = (e.c - x2) / e.b
      }
      if (x2 < maxBox.minX) {
        x2 = maxBox.minX
        y2 = (e.c - x2) / e.b
      }
    }
    else {
      x1 = if (s1 != null && s1.coord.x > maxBox.minX && s1.coord.x <= maxBox.maxX) {
        s1.coord.x
      } else if (s1 != null && s1.coord.x > maxBox.maxX) {
        maxBox.maxX
      } else {
        maxBox.minX
      }
      y1 = e.c - e.a * x1
      x2 = if (s2 != null && s2.coord.x >= maxBox.minX && s2.coord.x < maxBox.maxX) {
        s2.coord.x
      } else if (s2 != null && s2.coord.x < maxBox.minX) {
        maxBox.minX
      } else {
        maxBox.maxX
      }
      y2 = e.c - e.a * x2
      if (((y1 > maxBox.maxY) & (y2 > maxBox.maxY)) | ((y1 < maxBox.minY) & (y2 < maxBox.minY))) {
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
    Some(new GraphEdge(x1, y1, x2, y2, e.regL.siteIndex, e.regR.siteIndex))
  }

  private def endpoint(e: Edge, lr: Side, s: Site, boundingBox: Box): Unit = {
    e.endPoints(lr.index) = s
    if (e.endPoints(lr.inverse.index) == null) {
      return
    }
    clip_line(e, boundingBox).foreach { graphEdge => allEdges.add(graphEdge) }
  }

  private def intersect(el1: Halfedge, el2: Halfedge): Option[Point] = {
    val e1: Edge = el1.ELedge
    val e2: Edge = el2.ELedge

    if (e1 == null || e2 == null) None
    else if (e1.regR == e2.regR) None
    else if (Math.abs(e1.discriminant(e2)) < 1.0e-10) None
    else {
      val d = e1.discriminant(e2)
      val xint = (e1.c * e2.b - e2.c * e1.b) / d
      val yint = (e2.c * e1.a - e1.c * e2.a) / d
      val (e, el) = if ((e1.regR.coord.y < e2.regR.coord.y) || (e1.regR.coord.y == e2.regR.coord.y && e1.regR.coord.x < e2.regR.coord.x)) {
        (e1, el1)
      } else {
        (e2, el2)
      }
      val right_of_site = xint >= e.regR.coord.x
      if ((right_of_site && el.ELpm == LE) || (!right_of_site && el.ELpm == RE)) {
        None
      } else {
        Some(Point(xint, yint))
      }
    }
  }

  private def voronoi_bd(sites: Array[Site], sqrtNrSites: Int, nrSites: Int, maxBox: Box, boundingBox: Box): Boolean = {
    var nvertices: Int = 0
    var newsite: Site = null
    var temp: Site = null
    var newintstar: Point = null
    val pqHash: PQHash = new PQHash(sqrtNrSites, boundingBox)
    val el: ELt = new ELt(sqrtNrSites, boundingBox)
    val siteIterator = sites.iterator
    val bottomsite = siteIterator.next()
    newsite = siteIterator.next()
    var keepLooping = true
    while (keepLooping) {
      if (!pqHash.isEmpty) {
        newintstar = pqHash.min
      }
      if (newsite != null && (pqHash.isEmpty || newsite.coord.y < newintstar.y || (newsite.coord.y == newintstar.y && newsite.coord.x < newintstar.x))) {
        val lbnd = el.leftbnd(newsite.coord)
        val rbnd = lbnd.ELright
        val bot: Site = rightreg(lbnd, bottomsite)
        val e = bisect(bot, newsite)
        val bisector = new Halfedge(e, LE)
        lbnd.insert(bisector)
        intersect(lbnd, bisector).foreach { p =>
          pqHash.delete(lbnd)
          pqHash.insert(lbnd, p, p.dist(newsite.coord))
        }
        val bisector2 = new Halfedge(e, RE)
        bisector.insert(bisector2)
        intersect(bisector2, rbnd).foreach { p =>
          pqHash.insert(bisector2, p, p.dist(newsite.coord))
        }
        if (siteIterator.hasNext) newsite = siteIterator.next()
        else newsite = null
      } else if (!pqHash.isEmpty) {
        val lbnd = pqHash.extractmin
        val llbnd = lbnd.ELleft
        val rbnd = lbnd.ELright
        val rrbnd = rbnd.ELright
        var bot: Site = leftreg(lbnd)
        var top: Site = rightreg(rbnd, bottomsite)
        val v = Site(lbnd.vertex, nvertices)
        nvertices += 1

        endpoint(lbnd.ELedge, lbnd.ELpm, v, maxBox)
        endpoint(rbnd.ELedge, rbnd.ELpm, v, maxBox)
        lbnd.delete()
        pqHash.delete(rbnd)
        rbnd.delete()
        val pm = if (bot.coord.y > top.coord.y) {
          temp = bot
          bot = top
          top = temp
          RE
        } else {
          LE
        }
        val e = bisect(bot, top)
        val bisector = new Halfedge(e, pm)
        llbnd.insert(bisector)
        endpoint(e, pm.inverse, v, maxBox)
        intersect(llbnd, bisector).foreach { p =>
          pqHash.delete(llbnd)
          pqHash.insert(llbnd, p, p.dist(bot.coord))
        }
        intersect(bisector, rrbnd).foreach { p =>
          pqHash.insert(bisector, p, p.dist(bot.coord))
        }
      } else {
        keepLooping = false
      }
    }
    var lbnd = el.ELleftend.ELright
    while (lbnd != el.ELrightend) {
      {
        clip_line(lbnd.ELedge, maxBox).foreach(graphEdge => allEdges.add(graphEdge))
      }
      lbnd = lbnd.ELright
    }
    true
  }
}