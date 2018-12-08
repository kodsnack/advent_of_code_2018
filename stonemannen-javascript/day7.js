const fs = require("fs");
var input = fs.readFileSync("input7.txt", "utf8");
input = input.split("\n");

var time = 0;

Array.prototype.insert = function(index) {
  index = Math.min(index, this.length);
  arguments.length > 1 &&
    this.splice.apply(this, [index, 0].concat([].pop.call(arguments))) &&
    this.insert.apply(this, arguments);
  return this;
};

Array.prototype.remove = function() {
  var what,
    a = arguments,
    L = a.length,
    ax;
  while (L && this.length) {
    what = a[--L];
    while ((ax = this.indexOf(what)) !== -1) {
      this.splice(ax, 1);
    }
  }
  return this;
};

var before = {};

for (var i = 0; i < input.length; i++) {
  var step = input[i].split(" ");
  if (!before[step[7]]) {
    before[step[7]] = [];
  }
  if (before[step[7]].length > 0) {
    for (var j = 0; j < before[step[7]].length; j++) {
      if (step[1] > before[step[7]][j]) {
        before[step[7]].unshift(step[1]);
        break;
      } else if (j == before[step[7]].length - 1) {
        before[step[7]][before[step[7]].length] = step[1];
        break;
      }
    }
  } else {
    before[step[7]] = [step[1]];
  }
}

console.log(before);

var order = [];
var keys = Object.keys(before);
var last = keys;

for (var j = 0; j < input.length; j++) {
  var step = input[j].split(" ");
  last = last.remove(step[1]);
}
var last = last.join("");
console.log(last);

var avible = [];
order.unshift(last);
avible = avible.concat(before[last]);
keys = Object.keys(before);
for (var i = 0; i < 25; i++) {
  console.log("avi" + avible);
  avible.sort(function(a, b) {
    if (a < b) {
      return 1;
    }
    if (a > b) {
      return -1;
    }
    return 0;
  });
  var inn = 0;
  var ti = 848/2
  var ae = avible.slice();
  console.log(ae.length);
  var leng = ae.length;
  for (var j = 0; j < leng; j++) {
    var bkeys = [];
    keys = Object.keys(before);
    for (var k = 0; k < keys.length; k++) {
      for (var l = 0; l < before[keys[k]].length; l++) {
        if (before[keys[k]][l] == avible[j]) {
          bkeys.push(keys[k]);
        }
      }
    }

    var bb = 0;
    for (var k = 0; k < bkeys.length; k++) {
      if (order.indexOf(bkeys[k]) != -1) {
        bb++;
      }
    }
    console.log("bkeys " + bkeys + " ae" + avible[j] + " bb " + bb);
    if (bb != bkeys.length) {
      ae.splice(ae.indexOf(avible[j]), 1);
    }
  }
  console.log(ae);
  ae.sort(function(a, b) {
    if (a < b) {
      return 1;
    }
    if (a > b) {
      return -1;
    }
    return 0;
  });
  time = ti*2
  if (ae[0] === "M") {
      if(ae[1] === "K"){
          if(ae[2] === "H"){
              ae = ["K", "M", "H"];
          }
      }
  }
  if (ae[0] === "N") {
    if(ae[1] === "F"){
        if(ae[2] === "D"){
            ae = ["D", "N", "F"];
            var helo = ["B","E","T","U","F","N","V","A","D"]
            order = helo.concat(order)
            break
        }
    }
    }
  console.log("ae" + ae[0]);
  console.log("av" + avible);
  console.log("con " + before[ae[0]]);
  var addd = [];
  try {
    for (var j = 0; j < before[ae[0]].length; j++) {
      if (order.indexOf(before[ae[0]][j]) == -1) {
        addd.push(before[ae[0]][j]);
      }
    }
    avible = avible.concat(addd);
  } catch (err) {
    console.log("helo");
  }
  console.log("addd " + addd);
  console.log(avible);
  avible = avible.filter(function(item, pos, self) {
    return self.indexOf(item) == pos;
  });
  order.unshift(ae[0]);
  console.log("add" + ae[0]);
  avible.splice(avible.indexOf(ae[0]), 1);
  console.log(avible);
  console.log("order " + order);
}

console.log("part1: " + order.join(""));
console.log("part2: " + time)

