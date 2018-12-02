const { expect } = require("chai");
const {
  calculateDuplicates,
  getChecksum,
  getMostSimiliar
} = require("../day2");
const fs = require("fs");
const day2input = fs.readFileSync("./day2.input").toString();

describe("day2", () => {
  console.log("DAY 2: part 1 checksum ", getChecksum(day2input));
  console.log("DAY 2: part 2 most similar ", getMostSimiliar(day2input));
  describe("calculate duplicate letters", () => {
    it("should return { two: 0, three: 0 } for abcdef", () => {
      const input = "abcdef";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 0, three: 0 });
    });

    it("should return { two: 1, three: 1 } for bababc", () => {
      const input = "bababc";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 1, three: 1 });
    });

    it("should return { two: 1, three: 0 } for abbcde", () => {
      const input = "abbcde";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 1, three: 0 });
    });

    it("should return { two: 0, three: 1 } for abcccd", () => {
      const input = "abcccd";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 0, three: 1 });
    });

    it("should return { two: 1, three: 0 } for aabcdd", () => {
      const input = "aabcdd";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 1, three: 0 });
    });

    it("should return { two: 1, three: 0 } for abcdee", () => {
      const input = "abcdee";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 1, three: 0 });
    });

    it("should return { two: 0, three: 1 } for ababab", () => {
      const input = "ababab";
      expect(calculateDuplicates(input)).to.deep.equal({ two: 0, three: 1 });
    });
  });

  describe("part 1: calculate checksum", () => {
    const input = "abcdef, bababc, abbcde, abcccd, aabcdd, abcdee, ababab";
    it("should return 12 for example input", () => {
      expect(getChecksum(input)).to.equal(12);
    });
  });
  describe("part 2: find inputs that differ by one character", () => {
    const input = "abcde, fghij, klmno, pqrst, fguij, axcye, wvxyz";
    it("should return fgij for example input", () => {
      expect(getMostSimiliar(input)).to.equal("fgij");
    });
  });
});
