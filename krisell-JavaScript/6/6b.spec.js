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

    let within = 0

    for (let i_x = boundary.low.x; i_x <= boundary.high.x; ++i_x) {
        for (let i_y = boundary.low.y; i_y <= boundary.high.y; ++i_y) {

            let md_sum = coordinates.map((coordinate, index) => 
                Math.abs(i_x - coordinate.x) + Math.abs(i_y - coordinate.y)
            ).sum()

            if (md_sum < 10000) {
                within++
            }
        }
    }

    return within
}