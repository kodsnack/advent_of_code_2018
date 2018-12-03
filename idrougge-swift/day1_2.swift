/* Advent of code 2018, day 1, part 2 in Swift 4 */

import Foundation

let changes = try! String(contentsOfFile: "day1.txt")
    .components(separatedBy: .newlines)
    .flatMap(Int.init)

var freq = 0
var freqs: Set<Int> = []

while true {
    if !changes.drop(while: { change in
        freq += change
        return freqs.insert(freq).inserted
        }).isEmpty {
        break
    }
}

print(freq)
