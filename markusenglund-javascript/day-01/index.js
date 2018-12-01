const { readFileSync } = require("fs");
const path = require("path");
const input = readFileSync(path.resolve(__dirname, "./input.txt"), "utf8")
  .trim()
  .split("\n")
  .map(n => parseInt(n));

function a() {
  const result = input.reduce((acc, val) => acc + val);
  return result;
}

function b() {
  let i = 0;
  let result;
  const frequencies = [0];
  while (!result) {
    const nextFrequency =
      frequencies[frequencies.length - 1] + input[i % input.length];
    if (frequencies.includes(nextFrequency)) {
      result = nextFrequency;
    }
    frequencies.push(nextFrequency);
    i += 1;
  }
  return result;
}

console.log(a());
console.log(b());
