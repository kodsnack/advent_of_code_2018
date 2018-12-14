const { expect } = require("chai");
const { part1, part2 } = require("../day5");
const fs = require("fs");
const day5input = fs.readFileSync("./day5.input").toString();

describe("day5", () => {
  describe("part1", () => {
    console.log("DAY 5: part 1", part1(day5input).length);
    console.log("DAY 5: part 2", part2(day5input));
    it("should return dabCBAcaDA for the input dabAcCaCBAcCcaDA", () => {
      const input = "dabAcCaCBAcCcaDA";
      const result = part1(input);
      expect(result).to.equal("dabCBAcaDA");
      expect(result.length).to.equal(10);
    });
  });

  describe("part2", () => {
    it("should return 4 for the input dabAcCaCBAcCcaDA", () => {
      const input = "dabAcCaCBAcCcaDA";
      const result = part2(input);
      expect(result).to.equal(4);
    });
  });
});
