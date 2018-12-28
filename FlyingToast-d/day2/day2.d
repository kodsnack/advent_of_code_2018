import std.stdio : writeln;
import std.file : readText;
import std.string : split;
import std.algorithm;

void main() {
	string[] lines = readText("day2.in").split("\n")[0 .. $-1];

	uint two;
	uint three;

	foreach (line; lines) {
		foreach (ch; line.uniqueChars) {
			if (line.count(ch) == 3) {
				three++;
				break;
			}
		}

		foreach (ch; line.uniqueChars) {
			if (line.count(ch) == 2) {
				two++;
				break;
			}
		}
	}

	writeln(two * three);
}

string[] uniqueChars(string str) {
	import std.range;
	return str.split("").sort.uniq.array;
}
