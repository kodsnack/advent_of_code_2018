const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')
const fs = require('fs')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

const T = new Tester(app)
const L = new Logger(true)

const cases = {}
// cases[``] = 0
T.basic(cases)

let reg = {
    0: 0,
    1: 0,
    2: 0,
    3: 0,
}

let operations = {
    addr: (a, b, c) => reg[c] = reg[a] + reg[b],
    addi: (a, b, c) => reg[c] = reg[a] + b,
    mulr: (a, b, c) => reg[c] = reg[a] * reg[b],
    muli: (a, b, c) => reg[c] = reg[a] * b,
    banr: (a, b, c) => reg[c] = reg[a] & reg[b],
    bani: (a, b, c) => reg[c] = reg[a] & b,
    borr: (a, b, c) => reg[c] = reg[a] | reg[b],
    bori: (a, b, c) => reg[c] = reg[a] | b,
    setr: (a, b, c) => reg[c] = reg[a],
    seti: (a, b, c) => reg[c] = a,
    gtir: (a, b, c) => reg[c] = a > reg[b],
    gtri: (a, b, c) => reg[c] = reg[a] > b,
    gtrr: (a, b, c) => reg[c] = reg[a] > reg[b],
    eqir: (a, b, c) => reg[c] = a == reg[b],
    eqri: (a, b, c) => reg[c] = reg[a] == b,
    eqrr: (a, b, c) => reg[c] = reg[a] == reg[b],
}

app(fs.readFileSync('./input1', 'utf8'))

function app (input) {	
    let lines = input.lines()
    let counter = 0
    for (let i = 0 ; i < lines.length; i += 3) {
        let [before, instruction, after] = lines.slice(i)

        let [_, a, b, c, d] = before.split(': ')[1].match(/\[(\d*), (\d*), (\d*), (\d*)\]/)
        reg = { 0: +a, 1: +b, 2: +c, 3: +d }

        instruction = instruction.split(' ').map(Number)
        ;[_, a, b, c, d] = after.split(': ')[1].match(/\[(\d*), (\d*), (\d*), (\d*)\]/)
        
        let count = 0
        for (let operation of Object.values(operations)) {
            let savedState = JSON.parse(JSON.stringify(reg))
            operation(...instruction.slice(1))

            if (reg[0] == a && reg[1] == b && reg[2] == c && reg[3] == d) {
                count += 1
            } 

            reg = savedState
        }

        if (count >= 3) {
            counter++
        }

    }

    console.log(counter)
    return counter
}