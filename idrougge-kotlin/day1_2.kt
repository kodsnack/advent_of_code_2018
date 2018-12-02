/* Advent of code 2018, day 1, part 2 in Kotlin 1.3 */
import java.io.File

fun main() {
	val fn = "day1.txt"
	val changes = File(fn).readLines().map(String::toInt).toMutableList()
	val seen = mutableSetOf<Int>()

	val seq = generateSequence(0) {
		val change = changes.removeAt(0)
		changes.add(change)
		it + change
	}
	println(seq.find{ !seen.add(it) })
}