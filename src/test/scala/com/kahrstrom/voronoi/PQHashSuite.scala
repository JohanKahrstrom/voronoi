package com.kahrstrom.voronoi

import org.scalatest.{Matchers, FlatSpec}

/**
 * Created by johankahrstrom on 08/11/15.
 */
class PQHashSuite extends FlatSpec with Matchers  {
  "PQHashSuite" should "return the one element inside it" in {
    val p = new PQHash(9, Box(-1.0, 1.0, -1.0, 1.0))
    val a = new Halfedge("a", UnusedSide)
    p.insert(a, Point(0.0, 0.0), 0.0)
    p.min

    p.isEmpty should equal(false)
    p.extractmin.name should equal ("a")
    p.isEmpty should equal(true)
  }

  it should "delete the one element inside it" in {
    val p = new PQHash(9, Box(-1.0, 1.0, -1.0, 1.0))
    val a = new Halfedge("a", UnusedSide)
    p.insert(a, Point(0.0, 0.0), 0.0)
    p.delete(a)
    p.isEmpty should equal (true)
  }

  def extractEdges(p: PQHash): List[String] = {
    var r = List[String]()

    while (!p.isEmpty) {
      val h = p.extractmin
      r = r :+ h.name
    }

    r
  }

  it should "delete the correct element inside it" in {
    val p = new PQHash(9, Box(-1.0, 1.0, -1.0, 1.0))
    val a = new Halfedge("a", UnusedSide)
    p.insert(a, Point(0.0, 0.00), 0.0)
    p.insert(new Halfedge("b", UnusedSide), Point(0.0, 0.01), 0.0)
    p.insert(new Halfedge("c", UnusedSide), Point(0.0, 0.02), 0.0)
    p.min

    p.delete(a)
    extractEdges(p) should equal (List("b", "c"))
  }

  it should "delete the correct element inside it 2" in {
    val p = new PQHash(9, Box(-1.0, 1.0, -1.0, 1.0))
    val b = new Halfedge("b", UnusedSide)
    p.insert(new Halfedge("a", UnusedSide), Point(0.0, 0.00), 0.0)
    p.insert(b, Point(0.0, 0.01), 0.0)
    p.insert(new Halfedge("c", UnusedSide), Point(0.0, 0.02), 0.0)
    p.min

    p.delete(b)
    extractEdges(p) should equal (List("a", "c"))
  }

  it should "delete the correct element inside it 3" in {
    val p = new PQHash(9, Box(-1.0, 1.0, -1.0, 1.0))
    val c = new Halfedge("c", UnusedSide)
    p.insert(new Halfedge("a", UnusedSide), Point(0.0, 0.00), 0.0)
    p.insert(new Halfedge("b", UnusedSide), Point(0.0, 0.00), 0.01)
    p.insert(c, Point(0.0, 0.02), 0.0)
    p.min

    p.delete(c)
    extractEdges(p) should equal (List("a", "b"))
  }
}
