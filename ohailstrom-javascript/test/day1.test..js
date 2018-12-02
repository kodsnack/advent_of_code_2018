const { expect } = require("chai");
const day1 = require("../day1");
const fs = require("fs");
const day1input = fs.readFileSync("./day1.input").toString();

describe("day1", () => {
  console.log("DAY 1: result part 1:", day1(day1input).result);
  describe("part1", () => {
    it("should return result 3 if given '+1, +1, +1'", () => {
      let input = "+1, +1, +1";
      let { result } = day1(input);
      expect(result).to.equal(3);
    });
    it("should return result 0 if given '+1 +1 -2'", () => {
      let input = "+1, +1, -2";
      let { result } = day1(input);
      expect(result).to.equal(0);
    });
    it("should return result -6 if given -1, -2, -3", () => {
      let input = "-1, -2, -3";
      let { result } = day1(input);
      expect(result).to.equal(-6);
    });
  });

  describe("part2", () => {
    it("should return 0 as firstDuplicate if given '+1, -1'", () => {
      let input = "+1, -1";
      let { firstDuplicate } = day1(input);
      expect(firstDuplicate).to.equal(0);
    });
    it("should return 10 as firstDuplicate if given '+3, +3, +4, -2, -4'", () => {
      let input = "+3, +3, +4, -2, -4";
      let { firstDuplicate } = day1(input);
      expect(firstDuplicate).to.equal(10);
    });
  });
});
