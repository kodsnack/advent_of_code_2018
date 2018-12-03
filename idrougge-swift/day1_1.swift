/* Advent of code 2018, day 1, part 1 in Swift 4 */

import Foundation

let freq = try! String(contentsOfFile: "day1.txt")
    .components(separatedBy: .newlines)
    .flatMap(Int.init)
    .reduce(0, +)

print(freq)