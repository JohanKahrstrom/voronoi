package com.kahrstrom.voronoi

import org.scalatest.{Matchers, FlatSpec}
import scala.collection.JavaConversions._

case class P(x: Double, y: Double)
case class E(p1: P, p2: P, site1: Int, site2: Int)

class VoronoiSuite extends FlatSpec with Matchers {
  def convertToSimpleEdges(graphEdges: Seq[GraphEdge]): Set[E] = {
    graphEdges.foldLeft(Set.empty[E]) { case (set, edge) =>
        set + E(P(edge.x1, edge.y1), P(edge.x2, edge.y2), edge.site1, edge.site2)
    }
  }

  "Voronoi" should "handle two vertical points" in {
    val voronoi = new Voronoi(0.00001)
    val graphEdges1: Seq[GraphEdge] = voronoi.generateVoronoi(Array(0.0, 0.0), Array(-1.0, 1.0), -2.0, 2.0, -2.0, 2.0).toSeq
    val graphEdges2: Seq[GraphEdge] = voronoi.generateVoronoi(Array(0.0, 0.0), Array(1.0, -1.0), -2.0, 2.0, -2.0, 2.0).toSeq

    convertToSimpleEdges(graphEdges1) should be (Set(E(P(-2.0, 0.0), P(2.0, 0.0), 0, 1)))
    convertToSimpleEdges(graphEdges2) should be (Set(E(P(-2.0, 0.0), P(2.0, 0.0), 1, 0)))
  }

  it should "handle two horizontal points" in {
    val voronoi = new Voronoi(0.00001)
    val graphEdges1: Seq[GraphEdge] = voronoi.generateVoronoi(Array(1.0, -1.0), Array(0.0, 0.0), -2.0, 2.0, -2.0, 2.0).toSeq
    val graphEdges2: Seq[GraphEdge] = voronoi.generateVoronoi(Array(-1.0, 1.0), Array(0.0, 0.0), -2.0, 2.0, -2.0, 2.0).toSeq

    convertToSimpleEdges(graphEdges1) should be (Set(E(P(0.0, -2.0), P(0.0, 2.0), 1, 0)))
    convertToSimpleEdges(graphEdges2) should be (Set(E(P(0.0, -2.0), P(0.0, 2.0), 0, 1)))
  }

  it should "handle four points in a square" in {
    val voronoi = new Voronoi(0.00001)
    val graphEdges: Seq[GraphEdge] = voronoi.generateVoronoi(Array(1.0, -1.0, -1.0, 1.0), Array(1.0, 1.0, -1.0, -1.0), -2.0, 2.0, -2.0, 2.0).toSeq

    // Note that E(P(0.0, 0.0), P(0.0, -0.0), 2, 0) should not be an edge, this is a bug
    convertToSimpleEdges(graphEdges) should be (Set(E(P(-2.0,0.0),P(-0.0,0.0),2,1), E(P(0.0,0.0),P(0.0,-0.0),2,0), E(P(0.0,-2.0),P(0.0,0.0),2,3), E(P(0.0,0.0),P(2.0,0.0),3,0), E(P(0.0,-0.0),P(0.0,2.0),1,0)) )
  }

  it should "generate simple voronoi list" in {
    val voronoi = new Voronoi(0.00001)
    val graphEdges: Seq[GraphEdge] = voronoi.generateVoronoi(Array(1.0, -1.0, 0.0), Array(1.0, 1.0, 0.0), -2.0, 2.0, -2.0, 2.0).toSeq


    convertToSimpleEdges(graphEdges) should be (Set(E(P(-2.0, -1.0), P(0.0, 1.0), 2, 1), E(P(0.0, 1.0), P(0.0, 2.0), 1, 0), E(P(2.0, -1.0), P(0.0, 1.0), 2, 0)))
  }
}
