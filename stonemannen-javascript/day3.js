const fs = require("fs");
var input = fs.readFileSync("input3.txt", "utf8");
input = input.split("\n");

var claimed = [];

for (var i = 0; i < input.length; i++) {
  input[i] = input[i].replace(",", " ");
  input[i] = input[i].replace(":", "");
  input[i] = input[i].replace("x", " ");
  input[i] = input[i].split(" ");
  var x = parseInt(input[i][4]);
  var y = parseInt(input[i][5]);
  for (var j = 0; j < x; j++) {
    for (var k = 0; k < y; k++) {
      claimed.push(
        (parseInt(input[i][2]) + j).toString() +
          "," +
          (parseInt(input[i][3]) + k).toString()
      );
    }
  }
}

function countDuplicates(original) {
  let counts = {},
    duplicate = 0;
  original.forEach(function(x) {
    counts[x] = (counts[x] || 0) + 1;
  });

  for (var key in counts) {
    if (counts.hasOwnProperty(key)) {
      counts[key] > 1 ? duplicate++ : duplicate;
    }
  }

  return duplicate;
}

console.log("part1: " + countDuplicates(claimed));

var uniq = claimed
.map((name) => {
  return {count: 1, name: name}
})
.reduce((a, b) => {
  a[b.name] = (a[b.name] || 0) + b.count
  return a
}, {})

var duplicates = Object.keys(uniq).filter((a) => uniq[a] > 1)

for (var i = 0; i < input.length; i++) {  
  var x = parseInt(input[i][4]);
  var y = parseInt(input[i][5]);
  var noClaim = true;
  for (var j = 0; j < x; j++) {
    for (var k = 0; k < y; k++) {
        var result =
        (parseInt(input[i][2]) + j).toString() +
        "," +
        (parseInt(input[i][3]) + k).toString();

        for(var l = 0; l < duplicates.length;l++){
            if(result == duplicates[l]){
                noClaim = false
            }
        }
    }
  }
  if(noClaim){
      console.log("part2: " + input[i][0]);
      break;
  }
}
