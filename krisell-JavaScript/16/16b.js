require('../modules/AugmentString')()
const fs = require('fs')

let reg = {
    0: 0,
    1: 0,
    2: 0,
    3: 0,
}

let opcodes = {
    0: 'bori',
    1: 'borr',
    2: 'seti',
    3: 'mulr',
    4: 'setr',
    5: 'addr',
    6: 'gtir',
    7: 'eqir',
    8: 'gtri',
    9: 'bani',
    10: 'muli',
    11: 'gtrr',
    12: 'banr',
    13: 'eqri',
    14: 'addi',
    15: 'eqrr',
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

app(fs.readFileSync('./input2', 'utf8'))

function app (input) {
    for (let line of input.lines()) {
        let instruction = line.split(' ').map(Number)
        let operation = opcodes[instruction[0]]
        operations[operation](...instruction.slice(1))
    }

    console.log(reg[0])
    return reg[0]
}
