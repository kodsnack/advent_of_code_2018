/* Advent of code 2018, day 2, part 1 in Swift 4 */

import Foundation

let ids = try! String(contentsOfFile: "day2.txt")
    .components(separatedBy: .newlines)

let counts = ids.map{ $0.map{ ($0,1) } }
    .map{ [Character:Int]($0, uniquingKeysWith: { $0 + $1 }) }
let both = [2, 3]
    .map{ nr in counts.map{ $0.contains{ $1 == nr } }
        .reduce(0){ $1 ? $0 + 1 : $0 }}
let checksum = both.reduce(1, *)
print(checksum)
