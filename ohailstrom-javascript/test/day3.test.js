const { expect } = require("chai");
const day3 = require("../day3");
const fs = require("fs");
const day3input = fs.readFileSync("./day3.input").toString();

describe("day3", () => {
  console.log(
    "DAY 3: Part 1 Overlaps:",
    day3(day3input, { width: 1000, height: 1000, noLog: true })
  );

  describe("part1", () => {
    it("should return 4 overlaps for example input", () => {
      const input = "#1 @ 1,3: 4x4 \n #2 @ 3,1: 4x4 \n #3 @ 5,5: 2x2";
      var result = day3(input, { width: 8, height: 8 });

      expect(result).to.equal(4);
    });
  });
});
