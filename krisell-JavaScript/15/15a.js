const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

const T = new Tester(app)
const L = new Logger(true)

const cases = {

}

app(`################################
#######.......###.#.G###########
#######...G#.####...#.##########
#######....#####....G.##########
######......#..#......##########
#########G..G..#.......#########
###########.........G..#########
############.#.........#########
###########..##.......##.##.####
########G...##...G#............#
#####..##...#........G.........#
#####.G##........G.GG.......E.##
#####..#......#####....#.......#
#####.G......#######.......E...#
#####...G...#########E.........#
#####.....G.#########.........##
####....G...#########.....#...##
#.##........#########.....#.#.##
#...........#########.....#.#..#
#.G.#..##E...#######..#####.#..#
##G.......#...#####...#####.##E#
#........#..........E.#####..#.#
##..#....#........#######..#...#
#.....##.#...E....########.....#
##..E.##...G..E..##########....#
###########.....###########..E.#
###########.....##########.....#
###########..#############.....#
##########..#################..#
##########.##################..#
#########..##################.##
################################`)

function app (input) {	
    Array.prototype.readSort = function (options = {}) {
        if (options.where === undefined) {
            return this.sort((a, b) => a.r == b.r ? a.c - b.c : a.r - b.r)    
        }

        let p = options.where

        return this.sort((a, b) => a[p].r == b[p].r ? a[p].c - b[p].c : a[p].r - b[p].r)
    }

    let map
    let ges
    let powers
    let deads

    for (let jj = 0; jj < 1000; ++jj) {
        powers = { E: 3 + jj, G: 3 }
        map = {}
        ges = []
        deads = []

        function getBestTargetInRange (ge) {
            let targetsInRange = ges.filter(ge2 => {
                if (ge2.type === ge.type) {
                    return false
                }

                if (ge2.r === ge.r && Math.abs(ge2.c - ge.c) === 1) {
                    return true
                }

                if (ge2.c === ge.c && Math.abs(ge2.r - ge.r) === 1) {
                    return true
                }

                return false
            })

            return targetsInRange.sort((a, b) => {
                if (a.hp == b.hp) {
                    if (a.r == b.r) {
                        return a.c - b.c
                    }

                    return a.r - b.r
                }

                return a.hp - b.hp
            })[0]
        }

        function isFree (c, r) {
            let found = ges.find(ge => ge.c == c && ge.r == r)
            if (found) {
                return false
            }

            return map[`${c},${r}`] == '.'
        }


            function attack(ge, target) {
                target.hp -= powers[ge.type]
                if (target.hp <= 0) {
                    die(target)
                    deads.push(target)
                }
            }

            function die (ge) {
                ges = ges.filter(_ge => ge !== _ge)
            }

            function move(ge) {
                let squares = []

                ges.forEach(ge2 => {
                    if (ge2.type === ge.type) {
                        return
                    }

                    if (isFree(ge2.c - 1, ge2.r)) {
                        squares.push({ c: ge2.c - 1, r: ge2.r })
                    }

                    if (isFree(ge2.c + 1, ge2.r)) {
                        squares.push({ c: ge2.c + 1, r: ge2.r })
                    }

                    if (isFree(ge2.c, ge2.r + 1)) {
                        squares.push({ c: ge2.c, r: ge2.r + 1 })
                    }

                    if (isFree(ge2.c, ge2.r - 1)) {
                        squares.push({ c: ge2.c, r: ge2.r - 1 })
                    }
                })

                let chosenSquare = squares.map(square => {
                    let paths = distanceTo(ge.c, ge.r, square)
                    let steps = paths[0].steps

                    return {
                        square,
                        paths,
                        steps,
                    }
                }).sort((a, b) => {
                    if (a.steps === b.steps) {
                        if (a.square.r === b.square.r) {
                            return a.square.c - b.square.c
                        }

                        return a.square.r - b.square.r
                    }

                    return a.steps - b.steps
                })[0]

                if (!chosenSquare) {
                    return
                }

                Object.assign(ge, chosenSquare.paths.readSort({ where: 'first' })[0].first)
            }

            function distanceTo(c_start, r_start, square) {
                let queue = [{ c: c_start, r: r_start, steps: 0, order: [] }]

                let visited = {}
                let finished = false
                let wins = []

                while (true) {
                    if (queue.length === 0) {
                        return wins.length ? wins : [{ square, steps: 3e10, first: { c: c_start, r: r_start } }]
                    }

                    let {c, r, steps, order} = queue.shift()

                    if (visited[`${c},${r}`]) {
                        continue
                    }

                    visited[`${c},${r}`] = true

                    if (c === square.c && r === square.r) {
                        wins.push({ square, steps, first: order[0] })
                        finished = true
                    }

                    if (finished) {
                        continue
                    }

                    if (isFree(c, r - 1)) {
                        queue.push({ c, r: r - 1, steps: steps + 1, order: [...order, { c, r: r - 1 }] } )
                    }

                     if (isFree(c - 1, r)) {
                        queue.push({ c: c - 1, r: r, steps: steps + 1, order: [...order, { c: c - 1, r: r }] } )
                    }

                      if (isFree(c + 1, r)) {
                        queue.push({ c: c + 1, r: r, steps: steps + 1, order: [...order, { c: c + 1, r: r }] } )
                    }

                    if (isFree(c, r + 1)) {
                        queue.push({ c, r: r + 1, steps: steps + 1, order: [...order, { c, r: r + 1 }] } )
                    }
                }
            }

        function game () {
            input.lines().forEach((line, r) => {
                line.split('').forEach((char, c) => {
                    map[`${c},${r}`] = char

                    if (char !== '.' && char !== '#') {
                        map[`${c},${r}`] = '.'
                        ges.push({ type: char, c, r, hp: 200 })
                    }
                })
            })

            let startingElfs = ges.filter(ge => ge.type === 'E').length

            for (let i = 0; true; ++i) {
                ges.readSort()

                deads = []   
                for (let ge of ges) {
                    if (deads.find(dead => dead === ge)) {
                        continue
                    }

                    if (ges.filter(g => g.type === 'G').length === 0 || ges.filter(g => g.type === 'E').length === 0) {
                        return i * ges.reduce((acc, ge) => acc + ge.hp, 0)
                    }

                    let bestTargetInRange = getBestTargetInRange(ge)

                    if (!bestTargetInRange) {
                        move(ge)
                    }

                    bestTargetInRange = getBestTargetInRange(ge)
                    if (bestTargetInRange) {
                        attack(ge, bestTargetInRange)    
                    }
                }
            }
        }

        let result = game()
        if (result) {
            console.log(result)
            return result
        }
    }
}