import std.stdio : writeln;
import std.file : readText;
import std.string : split;
import std.conv;

void main() {
	string[] lines = readText("day1.in").split("\n")[0 .. $-1];

	int total;

	foreach (line; lines) {
		if (line[0] == '+') {
			total += line[1 .. $].to!int;
		} else {
			total -= line[1 .. $].to!int;
		}
	}

	writeln(total);
}
