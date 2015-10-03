package com.kahrstrom.voronoi

import org.scalatest.{Matchers, FlatSpec}
import scala.collection.JavaConversions._

case class P(x: Double, y: Double)
case class E(p1: P, p2: P, site1: Int, site2: Int)

class VoronoiSuite extends FlatSpec with Matchers {
  def convertToSimpleEdges(graphEdges: List[GraphEdge]): Set[E] = {
    graphEdges.foldLeft(Set.empty[E]) { case (set, edge) =>
        set + E(P(edge.x1, edge.y1), P(edge.x2, edge.y2), edge.site1, edge.site2)
    }
  }

  "Voronoi" should "generate simple voronoi list" in {
    val voronoi = new Voronoi(0.00001)
    val graphEdges: List[GraphEdge] = voronoi.generateVoronoi(Array(1.0, -1.0, 0.0), Array(1.0, 1.0, 0.0), -2.0, 2.0, -2.0, 2.0).toList


    convertToSimpleEdges(graphEdges) should be (Set(E(P(-2.0, -1.0), P(0.0, 1.0), 2, 1), E(P(0.0, 1.0), P(0.0, 2.0), 1, 0), E(P(2.0, -1.0), P(0.0, 1.0), 2, 0)))
  }
}
