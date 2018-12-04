const { readFileSync } = require("fs");
const path = require("path");
let input = readFileSync(path.resolve(__dirname, "./input.txt"), "utf8")
  .trim()
  .split("\n");

function a() {
  function filterByLetterAmount(amount) {
    return input.filter(id => {
      const letters = id.split("");
      const map = {};
      letters.forEach(letter => {
        map[letter] = map[letter] + 1 || 1;
      });
      return letters.find(letter => {
        return map[letter] === amount;
      });
    });
  }
  return filterByLetterAmount(2).length * filterByLetterAmount(3).length;
}

function b() {
  for (let i = 0; i < input.length; i += 1) {
    for (let j = i + 1; j < input.length; j += 1) {
      const identicalLetters = [];
      for (let k = 0; k < input[0].length; k += 1) {
        if (input[i][k] === input[j][k]) {
          identicalLetters.push(input[i][k]);
        }
      }
      if (identicalLetters.length === input[0].length - 1) {
        return identicalLetters.join("");
      }
    }
  }
}
console.log(a());
console.log(b());
