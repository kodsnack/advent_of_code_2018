const { expect } = require("chai");
const { part1, part2 } = require("../day3");
const fs = require("fs");
const day3input = fs.readFileSync("./day3.input").toString();

describe("day3", () => {
  console.log(
    "DAY 3: Part 1 Overlaps:",
    part1(day3input, { width: 1000, height: 1000, noLog: true })
  );
  console.log(
    "DAY 3: Part 2 ID:",
    part2(day3input, { width: 1000, height: 1000, noLog: true })
  );

  describe("part1", () => {
    it("should return 4 overlaps for example input", () => {
      const input = "#1 @ 1,3: 4x4 \n #2 @ 3,1: 4x4 \n #3 @ 5,5: 2x2";
      var result = part1(input, { width: 8, height: 8, noLog: true });
      expect(result).to.equal(4);
    });
  });

  describe("part2", () => {
    it("should return id 3 for example input", () => {
      const input = "#1 @ 1,3: 4x4 \n #2 @ 3,1: 4x4 \n #3 @ 5,5: 2x2";
      var result = part2(input, { width: 8, height: 8, noLog: true });
      expect(result).to.equal(3);
    });
  });
});
