const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

// const Grid = require('../modules/GridMover')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

const cases = {

}
cases[``] = 0
T.basic(cases)

function app (input) {  
    let coordinates = input.lines()
        .map(line => line.split(', '))
        .map(pair => ({ x: Number(pair[0]), y: Number(pair[1]) }))

    let sorted = {
        x: [...coordinates.sort((a, b) => a.x - b.x)],
        y: [...coordinates.sort((a, b) => a.y - b.y)],
    }

    let boundary = {
        low: { x: sorted.x[0].x, y: sorted.y[0].y },
        high: { x: sorted.x[sorted.x.length - 1].x, y: sorted.y[sorted.y.length - 1].y },
    }
    
    let closest = {}

    for (let i_x = boundary.low.x; i_x <= boundary.high.x; ++i_x) {
        for (let i_y = boundary.low.y; i_y <= boundary.high.y; ++i_y) {

            let mds = coordinates.map((coordinate, index) => {
                return {
                    dist: Math.abs(i_x - coordinate.x) + Math.abs(i_y - coordinate.y),
                    coords: { x: i_x, y: i_y },
                    index: index,
                }
            })

            let [shortest, second] = mds.sort((a, b) => a.dist - b.dist)
            if (shortest.dist === second.dist) {
                continue
            }

            let value = 1

            if (shortest.coords.x == boundary.low.x || shortest.coords.x == boundary.high.x || shortest.coords.y == boundary.low.y || shortest.coords.y == boundary.high.y) {
                value = 1e8 // "Punish" edges, to make them "infinite"
                // A prettier solution would be to use a dirty-flag, that lexicographically sorts these as worse
            }

            closest[shortest.index] = (closest[shortest.index] || 0) + value
        }
    }

    return Object.values(closest).sort((a, b) => b - a).filter(val => val < 1e8)[0]
}