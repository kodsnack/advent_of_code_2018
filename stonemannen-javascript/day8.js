const fs = require("fs");
var input = fs.readFileSync("input8.txt", "utf8");
input = input.split(" ");

var metadata = 0;

nodes(0);

function nodes(startPos) {
  var childnodes = parseInt(input[startPos]);
  var metaLength = parseInt(input[++startPos]);
  for (var j = childnodes; j > 0; --j) {
    startPos = nodes(++startPos);
  }
  for (var i = 0; i < metaLength; ++i) {
    metadata += parseInt(input[++startPos]);
  }
  return startPos;
}

console.log("part1 " + metadata);

var result = nodepart2(0);
console.log("part2 " + result[1]);

function nodepart2(startPos) {
  var value = 0;
  var start = startPos;
  var childnodes = parseInt(input[startPos]);
  var metaLength = parseInt(input[++startPos]);
  var metas = [];
  var starts = [];
  //console.log("child " + childnodes);
  //console.log("metalen " + metaLength);
  //console.log("startpos" + startPos);

  for (var j = 0; j < childnodes; ++j) {
    starts.push(startPos + 1);
    var re = nodepart2(++startPos);
    startPos = re[0];
  }
  if (childnodes > 0) {
    //console.log(starts);
    for (var i = 0; i < metaLength; ++i) {
      metas.push(parseInt(input[++startPos]));
    }
    //console.log("metas " + metas);
    //startPos = start;
    for (var j = 0; j < metas.length; ++j) {
      var res = [];
      if (starts[metas[j] - 1]) {
        res = nodepart2(starts[metas[j] - 1]);
        value += res[1];
      }
      //console.log("add malue " + res[1]);
    }
  } else {
    for (var i = 0; i < metaLength; ++i) {
      value += parseInt(input[++startPos]);
    }
  }

  //console.log("startpos " + startPos);
  //console.log("value " + value);
  return [startPos, value];
}
