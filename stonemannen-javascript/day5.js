const fs = require("fs");
var inputs = fs.readFileSync("input5.txt", "utf8");
inputs = inputs.split("");

var inputi = inputs
for(var i = 1; i < inputi.length; i++){
    var b = inputi[i].toLowerCase()
    var a = inputi[i-1].toUpperCase()
    var c = inputi[i-1].toLowerCase()
    var d = inputi[i].toUpperCase()
    if(a === inputi[i]){
        if(inputi[i-1] === b){
            inputi.splice(i-1, 2);
            i = 0
        }
    }else if(c === inputi[i]){
        if(d === inputi[i-1]){
            inputi.splice(i-1, 2);
            i = 0
        }

    }
}

console.log("part1:" + inputi.length)

var alphabet = "abcdefghijklmnopqrstuvwxyz";
alphabet = alphabet.split("");

var lengths = [];

for (var j = 0; j < alphabet.length; j++) {
  var input = fs.readFileSync("input5.txt", "utf8");
  input = input.split("");
  console.log(alphabet[j])
  for (var k = 0; k < input.length; k++) {
    if (input[k] === alphabet[j] || input[k] === alphabet[j].toUpperCase()) {
      input.splice(k, 1);
      k = 0
    }
  }

  for (var i = 1; i < input.length; i++) {
    var b = input[i].toLowerCase();
    var a = input[i - 1].toUpperCase();
    var c = input[i - 1].toLowerCase();
    var d = input[i].toUpperCase();
    if (a === input[i]) {
      if (input[i - 1] === b) {
        input.splice(i - 1, 2);
        i = 0;
      }
    } else if (c === input[i]) {
      if (d === input[i - 1]) {
        input.splice(i - 1, 2);
        i = 0;
      }
    }
  }
  lengths.push(input.length)

}

var shortest = inputs.length;

for (var i = 0; i < lengths.length; i++) {
  if (lengths[i] < shortest) {
    shortest = lengths[i];
  }
}

console.log("part2:" + shortest);
