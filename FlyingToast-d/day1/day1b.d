import std.stdio : writeln;
import std.file : readText;
import std.string : split;
import std.conv;

void main() {
	string[] lines = readText("day1.in").split("\n")[0 .. $-1];

	bool[int] pastTotals;
	int total;

	while (true) {
		foreach (line; lines) {
			if (line[0] == '+') {
				total += line[1 .. $].to!int;
			} else {
				total -= line[1 .. $].to!int;
			}

			if ((total in pastTotals) !is null) {
				writeln(total);
				return;
			}

			pastTotals[total] = true;
		}
	}
}
