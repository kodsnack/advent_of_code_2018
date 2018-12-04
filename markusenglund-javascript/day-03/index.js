const { readFileSync } = require("fs");
const path = require("path");
let input = readFileSync(path.resolve(__dirname, "./input.txt"), "utf8")
  .trim()
  .split("\n")
  .map(n => n.split(/\s+/))
  .map(arr => {
    const pos = arr[2]
      .replace(":", "")
      .split(",")
      .map(Number);
    const width = Number(arr[3].split("x")[0]);
    const height = Number(arr[3].split("x")[1]);
    return [pos, width, height];
  });

function a() {
  const map = {};
  input.forEach(arr => {
    for (let i = arr[0][0]; i < arr[0][0] + arr[1]; i += 1) {
      for (let j = arr[0][1]; j < arr[0][1] + arr[2]; j += 1) {
        const id = `${i},${j}`;
        map[id] = map[id] ? map[id] + 1 : 1;
      }
    }
  });
  return Object.values(map).filter(val => val > 1).length;
}

function b() {
  const map = {};
  input.forEach(arr => {
    for (let i = arr[0][0]; i < arr[0][0] + arr[1]; i += 1) {
      for (let j = arr[0][1]; j < arr[0][1] + arr[2]; j += 1) {
        const id = `${i},${j}`;
        map[id] = map[id] ? map[id] + 1 : 1;
      }
    }
  });
  let id;
  input.forEach((arr, i) => {
    for (let i = arr[0][0]; i < arr[0][0] + arr[1]; i += 1) {
      for (let j = arr[0][1]; j < arr[0][1] + arr[2]; j += 1) {
        const id = `${i},${j}`;
        if (map[id] > 1) {
          return;
        }
      }
    }
    id = i + 1;
  });
  return id;
}

console.log(b());
