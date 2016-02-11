package com.kahrstrom.voronoi

/*
 * An arc on the beach line.
 */
class Arc(val halfEdge: Halfedge) {
  var left: Arc = _
  var right: Arc = _
  var deleted: Boolean = false

  def insert(that: Arc) {
    that.left = this
    that.right = this.right
    this.right.left = that
    this.right = that
  }

  def delete(): Unit = {
    this.left.right = this.right
    this.right.left = left
    this.deleted = true
  }
}

/*
 * Stores the arcs in a 'hash' (really just an array of linked
 * lists).
 */
class BeachLine(sqrt_nsites: Int, boundingBox: Box) {
  private val ELhashsize: Int = 2 * sqrt_nsites
  private val ELhash: Array[Arc] = new Array[Arc](ELhashsize)
  val ELleftend: Arc = new Arc(new Halfedge("leftend", null, LE))
  val ELrightend: Arc = new Arc(new Halfedge("rightend", null, LE))
  ELleftend.left = null
  ELleftend.right = ELrightend
  ELrightend.left = ELleftend
  ELrightend.right = null
  ELhash(0) = ELleftend
  ELhash(ELhashsize - 1) = ELrightend

  // TODO: Very inefficient, optimise
  def find(he: Halfedge): Arc = {
    var bucket = 0
    while (bucket < ELhash.length) {
      var node = ELhash(bucket)
      while (node != null) {
        if (node.halfEdge == he) return node
        else node = node.right
      }
      bucket += 1
    }
    null
  }

  private def get(b: Int): Arc = {
    val he: Arc = ELhash(b)
    if (he == null || !he.deleted) he
    else {
      ELhash(b) = null
      null
    }
  }

  private def getBucket(p: Point): Int = {
    val bucket = ((p.x - boundingBox.minX) / (boundingBox.maxX - boundingBox.minX) * ELhashsize).toInt
    if (bucket < 0) 0
    else if (bucket >= ELhashsize) ELhashsize - 1
    else bucket
  }

  def getHalfedge(bucket: Int): Arc = {
    var he = get(bucket)
    if (he == null) {
      var i: Int = 1
      while (i < ELhashsize && he == null) {
        if (get(bucket - i) != null) he = get(bucket - i)
        else if (get(bucket + i) != null) he = get(bucket + i)
        i += 1
      }
    }
    he
  }

  def leftbnd(p: Point): Arc = {
    val bucket: Int = getBucket(p)
    var he: Arc = getHalfedge(bucket)
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

    val e: Edge = el.edge
    val right_of_site: Boolean = p.x > e.siteR.coord.x
    if (right_of_site && el.pm == LE) true
    else if (!right_of_site && el.pm == RE) false
    else if (el.pm == LE) above(e, right_of_site)
    else !above(e, right_of_site)
  }
}