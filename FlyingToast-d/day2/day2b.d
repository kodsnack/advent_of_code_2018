import std.stdio : writeln;
import std.file : readText;
import std.string : split;

void main() {
	string[] lines = readText("day2.in").split("\n")[0 .. $-1];

	foreach (lineA; lines) {
		foreach (lineB; lines) {
			if (lineA == lineB || lineA.length != lineB.length) {
				continue;
			}

			if (lineA.length - commonChars(lineA, lineB).length == 1) {
				writeln(commonChars(lineA, lineB));
				return;
			}
		}
	}
}

string commonChars(string a, string b) {
	string common;

	foreach (i; 0 .. a.length) {
		if (a[i] == b[i]) {
			common ~= a[i];
		}
	}

	return common;
}
