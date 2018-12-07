// Advent of Code 2018
// Peter Westerström (digimatic)
import scala.language.postfixOps

def isPolarized(x : Char, y : Char) : Boolean = {
	x.toLower == y.toLower && ( x.isLower && y.isUpper || x.isUpper && y.isLower )
}

def react(p : List[Char]) : List[Char] = {
	def react2(rs : List[Char], q : List[Char]) : List[Char] = q match {
		case x :: y :: ps => if (isPolarized(x,y)) {
				if(rs isEmpty)
					react2(rs, ps)
				else
					react2(rs.tail, rs.head :: ps)
			} else {
				react2(x::rs, y :: ps)
			}
		case x::Nil => react2(x::rs, Nil)
		case Nil => rs
	}
	react2(Nil, p).reverse
}

val polymerDemo="dabAcCaCBAcCcaDA";
/*val r=react(polymerDemo.toList).mkString
r.length
r=="dabCBAcaDA"*/

val polymer = scala.io.Source.fromFile("input.txt", "utf-8").getLines.mkString("\n")
val l = react(polymer.toList).length
l==11754
println(s"Day 5 part 1 answer: ${l}")

//
// part 2
//
val polys = ('a' to 'z' toList) map ( x=> (polymer toList) filter ( y => x.toUpper != y.toUpper ) )
val ml = ( (polys map react) map (x=>x.length) ) min

println(s"Day 5 part 2 answer: ${ml}")
