const fs = require("fs");
var input = fs.readFileSync("input1.txt", "utf8");
input = input.split("\n");

var result = 0;
var list = [];
list.push(result);
var first;
var part1;

for (var i = 0; i < input.length; i++) {
  //console.log(list);
  //console.log(parseInt(input[i]));
  result += parseInt(input[i]);
  //console.log(result);
  list.push(result);

  if (i == input.length - 1) {
    if (part1 == undefined) {
      part1 = result;
      console.log(part1)
    }
    if (first == undefined||first == -1) {
        first = firstDuplicate(list)
        i = -1;
    }
  }
}
console.log("part1: " + part1);
console.log("part2: " + first)
//console.log(list);

function firstDuplicate(arr) {
    var dictionary = {};
  
    for(var i = 0; i < arr.length; i++) {
  if(dictionary[arr[i]] !== undefined)
       return arr[i];
  else
     dictionary[arr[i]] = i;
    }
  
    return -1;
  }
