const fs = require("fs");
var input = fs.readFileSync("input2.txt", "utf8");
input = input.split("\n");
var doubles = 0;
var triples = 0;
for (var i = 0; i < input.length; i++) {
  var string = input[i].split("");
  for (var j = 0; j < string.length; j++) {
    if (input[i].split(string[j]).length - 1 == 2) {
      doubles++;

      break;
    }
  }
  for (var j = 0; j < string.length; j++) {
    if (input[i].split(string[j]).length - 1 == 3) {
      triples++;
      break;
    }
  }
}
console.log("part1: " + doubles * triples);


