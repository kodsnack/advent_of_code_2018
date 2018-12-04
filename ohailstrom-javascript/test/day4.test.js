const { expect } = require("chai");
const {part1, part2} = require("../day4");
const fs = require("fs");
const day4input = fs.readFileSync("./day4.input").toString();


const exampleInput = `[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up`;

describe('day4', () => {
	console.log("DAY 4: part 1", part1(day4input));
	console.log("DAY 4: part 2", part2(day4input));
	
	describe('part1', () => {
		it('should return 240 for example input', () => {
			const result = part1(exampleInput);
			expect(result).to.equal(240);
		})
	})
	describe('part2', () => {
		it('should return 4455 for example input', () => {
			const result = part2(exampleInput);
			expect(result).to.equal(4455);
		})
	})
})
