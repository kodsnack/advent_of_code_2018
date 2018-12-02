/* Advent of code 2018, day 1, part 1 in Kotlin 1.3 */
import java.io.File

fun main() {
	val fn = "day1.txt"
	val changes = File(fn).readLines().map(String::toInt)
	val freq = changes.fold(0, Int::plus)
	println(freq)
}