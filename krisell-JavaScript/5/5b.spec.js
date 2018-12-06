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
    let results = []
    let alphabet = 'abcdefghijklmnopqrstuvwxyz'.split('')

    alphabet.forEach(char => {
        let base = input.replace(new RegExp(`${char}`, 'ig'), '')
        
        while (true) {
            let size = base.length

            alphabet.forEach(char => {
                let reg = new RegExp(`${char}${char.toUpperCase()}|${char.toUpperCase()}${char}`, 'g')
                base = base.replace(reg, '')
            })

            if (base.length === size) {
                results.push(base.length)
                return
            }
        }
    })

    return results.sort((a, b) => a - b)[0]
}