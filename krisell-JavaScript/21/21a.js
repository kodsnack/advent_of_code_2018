const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

app(`#ip 1
seti 123 0 4
bani 4 456 4
eqri 4 72 4
addr 4 1 1
seti 0 0 1
seti 0 1 4
bori 4 65536 3
seti 3730679 4 4
bani 3 255 5
addr 4 5 4
bani 4 16777215 4
muli 4 65899 4
bani 4 16777215 4
gtir 256 3 5
addr 5 1 1
addi 1 1 1
seti 27 1 1
seti 0 0 5
addi 5 1 2
muli 2 256 2
gtrr 2 3 2
addr 2 1 1
addi 1 1 1
seti 25 1 1
addi 5 1 5
seti 17 1 1
setr 5 2 3
seti 7 6 1
eqrr 4 0 5
addr 5 1 1
seti 5 1 1`)

function app (input) {	
    let reg = [0, 0, 0, 0, 0, 0]

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
        gtir: (a, b, c) => reg[c] = +(a > reg[b]),
        gtri: (a, b, c) => reg[c] = +(reg[a] > b),
        gtrr: (a, b, c) => reg[c] = +(reg[a] > reg[b]),
        eqir: (a, b, c) => reg[c] = +(a == reg[b]),
        eqri: (a, b, c) => reg[c] = +(reg[a] == b),
        eqrr: (a, b, c) => reg[c] = +(reg[a] == reg[b]),
    }

    let instructions = input.split(/\n+/).slice(1)
    let ip = 0

    while (ip < instructions.length) {
        if (ip === 28) {
            console.log(reg[4])
            process.exit()
        }

        reg[1] = ip

        let instr = instructions[ip].match(/(.*) (\d*) (\d.*) (\d.*)/)
        operations[instr[1]](...instr.slice(2).map(Number))

        ip = reg[1] + 1
    }
}