import std.stdio : writeln;
import std.file : readText;
import std.string;

void main() {
	string[] lines = readText("day3.in").split("\n")[0 .. $-1];
	Claim[] claims;
	uint[uint][uint] inches;

	foreach (line; lines) {
		auto claim = new Claim(line);
		claims ~= claim;
		foreach (x; 0 .. claim.width) {
			foreach (y; 0 .. claim.height) {
				inches[claim.fromLeft + x][claim.fromTop + y]++;
			}
		}
	}

	foreach (claim; claims) {
		if (doesClaimNotOverlap(inches, claim)) {
			writeln(claim.id);
		}
	}
}

bool doesClaimNotOverlap(uint[uint][uint] inches, Claim claim) {
	foreach (x; 0 .. claim.width) {
		foreach (y; 0 .. claim.height) {
			if (inches[claim.fromLeft + x][claim.fromTop + y] != 1) {
				return false;
			}
		}
	}
	return true;
}

class Claim {
	string stringOf;
	uint id;
	uint fromTop;
	uint fromLeft;
	uint width;
	uint height;

	this(string str) {
		import std.conv;

		this.stringOf = str;
		string[] parts = str.replace("#", "").replace("@ ", "").replace(":", "").split(" ");
		this.id = parts[0].to!uint;
		string[] location = parts[1].split(",");
		this.fromLeft = location[0].to!uint;
		this.fromTop = location[1].to!uint;
		string[] dimensions = parts[2].split("x");
		this.width = dimensions[0].to!uint;
		this.height = dimensions[1].to!uint;
	}
}
