const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

// require('../modules/AugmentArray')()
// require('../modules/AugmentString')()

// const Grid = require('../modules/GridMover')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

const cases = {
}
cases[``] = 0
T.basic(cases)

function app (input) {  
    while (true) {
        let size = input.length

        for (let char of 'abcdefghijklmnopqrstuvwxyz') {
            let reg = new RegExp(`${char}${char.toUpperCase()}|${char.toUpperCase()}${char}`, 'g')
            input = input.replace(reg, '')
        }

        if (input.length === size) {
            return input.length
        }
    }
}