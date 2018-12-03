const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

// const Grid = require('../modules/GridMover')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

const cases = {
    // '': 0
}
// cases[``] = 0
T.basic(cases)


function app (input) {  
    let claims = input.lines()
        .map(line => line.split('@')[1].trim())
        .map(claim => claim.split(': '))
        .map(part => {
            let [x, y] = part[0].split(',').map(Number)
            let [w, h] = part[1].split('x').map(Number)
            return {x, y, w, h}
        })

    let grid = {}

    for (let [index, { x, y, w, h }] of claims.entries()) {
        for (let i = x; i < x + w; ++i) {
            for (let j = y; j < y + h; ++j) {
                let claims = grid[`${i},${j}`] || []
                claims.push(index)
                grid[`${i},${j}`] = claims
            }
        }
    }

    return Object.values(grid).map(claim => claim.length > 1 ? 1 : 0).sum()
}