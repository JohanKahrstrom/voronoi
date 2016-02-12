package com.kahrstrom.voronoi

import com.kahrstrom.javavoronoi
import org.scalatest.{Matchers, FlatSpec}
import scala.collection.JavaConversions._
import scala.util.Random

case class P(x: Double, y: Double)
case class E(p1: P, p2: P, site1: Int, site2: Int)

class VoronoiSuite extends FlatSpec with Matchers {
  def convertToScala(e: javavoronoi.GraphEdge): GraphEdge = {
    GraphEdge(e.x1, e.y1, e.x2, e.y2, e.site1, e.site2)
  }

  // class GraphEdge(val x1: Double, val y1: Double, val x2: Double, val y2: Double, val site1: Int, val site2: Int)

  def convertToSimpleEdges(graphEdges: Seq[GraphEdge]): Set[E] = {
    graphEdges.foldLeft(Set.empty[E]) { case (set, edge) =>
        set + E(P(edge.x1, edge.y1), P(edge.x2, edge.y2), edge.site1, edge.site2)
    }
  }

  def round(d: Double): Double = BigDecimal(d).setScale(8, BigDecimal.RoundingMode.HALF_UP).toDouble
  def round(p: P): P = P(round(p.x), round(p.y))
  def round(e: E): E = E(round(e.p1), round(e.p2), e.site1, e.site2)
  def round(s: Set[E]): Set[E] = s.map(round)

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

  it should "generate three points list" in {
    val voronoi = new Voronoi(0.00001)
    val graphEdges: Seq[GraphEdge] = voronoi.generateVoronoi(Array(1.0, -1.0, 0.0), Array(1.0, 1.0, 0.0), -2.0, 2.0, -2.0, 2.0).toSeq

    convertToSimpleEdges(graphEdges) should be (Set(E(P(-2.0, -1.0), P(0.0, 1.0), 2, 1), E(P(0.0, 1.0), P(0.0, 2.0), 1, 0), E(P(2.0, -1.0), P(0.0, 1.0), 2, 0)))
  }

  it should "generate complex voronoi list" in {
    val voronoi = new Voronoi(0.00001)
    val xs: Array[Double] = Array(0.6670633210343437, 0.05629359362719377, -0.7314012279761979, 0.27191006044145727, 1.758901893004146, -1.7748209622289064, -0.864919260613136, -1.5871393180122815, -1.1893939731929164, 1.32803088026187)
    val ys: Array[Double] = Array(0.09546412796829529, 0.4352870634566086, 1.6734217249107157, -0.6765828276749302, 0.8941254122164377, 1.2680571288927691, -1.4386385303500102, 1.9353539198847929, 1.372996956517527, 1.6936849453545504)
    val result: Seq[GraphEdge] = voronoi.generateVoronoi(xs, ys, -2.0, 2.0, -2.0, 2.0).toSeq
    val expected: Set[E] = Set(E(P(-0.7993421076417856, -0.307481232688594), P(0.1448205341269588, -0.1243869565147698), 3, 1),
      E(P(0.1448205341269588, -0.1243869565147698), P(2.0, -1.0739149736467604), 3, 0),
      E(P(-1.2791596948386679, -0.0716052029907136), P(-0.7993421076417856, -0.307481232688594), 6, 1),
      E(P(-1.279159694838668, -0.07160520299071359), P(-1.2457365413067285, 0.0018864190969437112), 1, 5),
      E(P(-1.2457365413067285, 0.0018864190969437112), P(-0.560011883601033, 0.9128277312096309), 1, 8),
      E(P(2.0, -0.5811256024300643), P(0.887155306164495, 0.9402286754167345), 0, 4),
      E(P(0.14482053412695906, -0.1243869565147698), P(0.7652149828385783, 0.9906588769279112), 0, 1),
      E(P(0.7652149828385783, 0.9906588769279112), P(0.8871553061644951, 0.9402286754167344), 0, 9),
      E(P(-1.2457365413067285, 0.0018864190969437112), P(-1.5246269519409832, 1.5577301541604363), 5, 8),
      E(P(-1.5246269519409832, 1.5577301541604365), P(-1.1546788724314556, 1.8193880263573188), 8, 7),
      E(P(-0.5600118836010333, 0.9128277312096309), P(-1.1546788724314556, 1.8193880263573188), 8, 2),
      E(P(-0.5600118836010332, 0.9128277312096308), P(0.30051158177844317, 1.4602882770209829), 1, 2),
      E(P(0.7652149828385785, 0.9906588769279112), P(0.30051158177844317, 1.4602882770209826), 1, 9),
      E(P(-1.1546788724314554, 1.8193880263573188), P(-1.0993954963077717, 2.0), 2, 7),
      E(P(0.30051158177844306, 1.4602882770209826), P(0.29520123550623595, 2.0), 2, 9),
      E(P(-2.0, -0.3139279103921874), P(-1.2791596948386679, -0.0716052029907136), 6, 5),
      E(P(-2.0, 1.6914319739209502), P(-1.5246269519409832, 1.5577301541604363), 5, 7),
      E(P(-0.11417779298397346, 2.0), P(-0.11417779298397346, 2.0), 9, 7),
      E(P(0.8871553061644951, 0.9402286754167345), P(2.0, 1.5399245084160096), 4, 9),
      E(P(2.0, -0.8233453375525477), P(2.0, -0.8233453375525477), 3, 4),
      E(P(0.33521129859668264, -2.0), P(-0.7993421076417856, -0.307481232688594), 6, 3))

    round(convertToSimpleEdges(result)) should be (round(expected))
  }

  val r = new Random()
  def scale(min: Double, max: Double, v: Double): Double = v * (max - min) + min

  def generateValues(min: Double, max: Double, n: Int): Array[Double] = {
    val ret: Array[Double] = new Array[Double](n)

    var i: Int = 0
    while (i < ret.length) {
      ret(i) = scale(min, max, r.nextDouble)
      i += 1
    }

    ret
  }

  def copyValues(values: Array[Double]): Array[Double] = {
    val ret: Array[Double] = new Array[Double](values.size)

    var i: Int = 0
    while (i < ret.length) {
      ret(i) = values(i)
      i += 1
    }

    ret
  }

  def runTests(nrTests: Int, nrValues: Int, min: Double, max: Double): Unit = {
    val jvoronoi = new javavoronoi.Voronoi(0.000001)
    val voronoi = new Voronoi(0.000001)

    for (i <- (0 to nrTests)) {
      val jxs = generateValues(min, max, nrValues)
      val jys = generateValues(min, max, nrValues)
      val xs = copyValues(jxs)
      val ys = copyValues(jys)

      val jret: Seq[javavoronoi.GraphEdge] = jvoronoi.generateVoronoi(jxs, jys, min, max, min, max).toSeq
      val ret = voronoi.generateVoronoi(xs, ys, min, max, min, max).toSeq

      val r = round(convertToSimpleEdges(jret.map(convertToScala)))
      val e = round(convertToSimpleEdges(ret))
      val d1 = r.diff(e)
      val d2 = e.diff(r)

      d1 should be (d2)
      d1 should be (Set.empty)
      r should be (e)
    }
  }

  it should "generate same edges as Java original on 10 points" in {
    runTests(100, 10, -2.0, 2.0)
  }

  it should "generate same edges as Java original on 100 points" in {
    runTests(30, 100, -3.0, 3.0)
  }

  it should "generate same edges as Java original on 1000 points" in {
    runTests(10, 1000, -4.0, 4.0)
  }

  it should "generate same edges as Java original on 10000 points" in {
    runTests(5, 10000, -5.0, 5.0)
  }
}
