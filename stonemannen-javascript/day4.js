const fs = require("fs");
var input = fs.readFileSync("input4.txt", "utf8");
input = input.split("\n");

input.sort();

var totalSleep = {};

var sleepTime = {};

var guard = "";

for (var i = 0; i < input.length; i++) {
  input[i] = input[i].replace("]", "");
  input[i] = input[i].split(" ");
  if (input[i][2] === "Guard") {
    guard = input[i][3];
    totalSleep[guard] = totalSleep[guard] ? totalSleep[guard] + 0 : 0;
    sleepTime[guard] = sleepTime[guard] ? sleepTime[guard] : [];
  }
  if (input[i][2] === "wakes") {
    var wake = input[i][1];
    wake = wake.split(":");

    var fall = input[i - 1][1];
    fall = fall.split(":");

    var timeAsleep = parseInt(wake[1]) - parseInt(fall[1]);
    for (var j = 0; j < timeAsleep; j++) {
      sleepTime[guard].push(parseInt(fall[1]) + j);
    }
    totalSleep[guard] += timeAsleep;
  }
}

function foo(arr) {
  var a = [],
    b = [],
    prev;

  arr.sort();
  for (var i = 0; i < arr.length; i++) {
    if (arr[i] !== prev) {
      a.push(arr[i]);
      b.push(1);
    } else {
      b[b.length - 1]++;
    }
    prev = arr[i];
  }

  return [a, b];
}

var guards = Object.keys(totalSleep);

var longest = 0;
var lguard;

for (var i = 0; i < guards.length; i++) {
  if (totalSleep[guards[i]] > longest) {
    longest = totalSleep[guards[i]];
    lguard = guards[i];
  }
}
var b = foo(sleepTime[lguard]);

var highest = 0;
var times = 0;
for (var i = 0; i < b[0].length; i++) {
  if (b[1][i] > highest) {
    times = b[0][i];
    highest = b[1][i];
  }
}

lguard = lguard.replace("#", "");
console.log("part1:" + parseInt(lguard) * times);

lguard = "";
var most = 0;
var freq = 0;
for (var j = 0; j < guards.length; j++) {
  var b = foo(sleepTime[guards[j]]);
  var highest = 0;
  var times = 0;
  for (var i = 0; i < b[0].length; i++) {
    if (b[1][i] > highest) {
      times = b[0][i];
      highest = b[1][i];
    }
  }
  if (highest > freq) {
    most = times;
    lguard = guards[j];
    freq = highest;
  }
}
lguard = lguard.replace("#", "");
console.log("part2:" + parseInt(lguard) * most);
