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

function findStringDiff(str1, str2) {
    var compareString = function(str1, str2) {
        var a1 = str1.split("");
        var a2 = str2.split("");
        var idx2 = 0;
        a1.forEach(function(val) {
            if (a2[idx2] === val) {
              a2.splice(idx2,1);
            } else {
                idx2 += 1;
            }
        });
        if (idx2 > 0) {
            a2.splice(idx2,a2.length);
        }
        return a2.join("");
    }

    if (str1.length < str2.length) {
        return compareString(str1, str2);
    } else {
        return compareString(str2, str1);
    }
}

for(var i = 0; i < input.length; i++){
    for(var j = i+1; j < input.length; j++){
        if(findStringDiff(input[i],input[j]).length == 1){
            console.log("part2: " + input[i].replace(findStringDiff(input[i],input[j]),""))
        }
    }
}
