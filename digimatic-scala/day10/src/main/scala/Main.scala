// Advent of Code 2018
// Peter Westerström (digimatic)
import scala.language.postfixOps

object Main extends App {
    def update(points : List[(Int,Int,Int,Int)]) : List[(Int,Int,Int,Int)] = points match {
        case (x, y, vx, vy) :: ps => (x+vx, y+vy, vx, vy) :: update(ps)
        case Nil => Nil
    }

    def boundingRect(points : List[(Int,Int,Int,Int)]) : ((Int,Int),(Int,Int)) = {
        ( (points.map(p => p._1).min, points.map(p => p._2).min),
          (points.map(p => p._1).max, points.map(p => p._2).max) )
    }

    def rectSize( r :  ((Int,Int),(Int,Int)) ) : Int = {
        (r._1._1 - r._2._1).abs + (r._1._2 - r._2._2).abs
    }

    def findMin(points : List[(Int,Int,Int,Int)]): (Int, List[(Int,Int,Int,Int)]) = {
        def _findMin(t : Int, points : List[(Int,Int,Int,Int)]): (Int, List[(Int,Int,Int,Int)]) = {
            val f1 = rectSize(boundingRect(points))
            val points2 = update(points)
            val f2 = rectSize(boundingRect(points2))
            if(f2 > f1)
                (t, points)
            else
                _findMin(t+1, points2)
        }
        _findMin(0, points)
    }

    def draw(points : List[(Int, Int, Int, Int)]): String = {
        val ((x0, y0), (x1, y1)) = boundingRect(points)
        val w = x1 - x0 + 1
        val h = y1 - y0 + 1
        val lineTemplate = Array.fill(w)('.')
        val canvas = Array.fill(h)(lineTemplate)
        def _draw(p : (Int,Int,Int,Int)) : Unit = {
            val (x, y, _, _) = p
            canvas(y-y0) = canvas(y-y0).updated(x-x0, '#')
        }
        points.map( p => _draw(p) )
        canvas.map( l => l.mkString ).mkString("\n")
    }

    val lines = scala.io.Source.fromFile("input.txt","utf-8").getLines.toList
    val points = lines.map(l => l.split("[^\\d-]+").tail.map(_.toInt)).map( p => (p(0),p(1),p(2),p(3) ))

    val (t, ps) = findMin(points)
    val drawing = draw(ps)

    println("Day 10 - part 1:")
    println(drawing)
    println(s"Day 10 - part 2: ${t}")
}
