app(`#ip 2
addi 2 16 2
seti 1 1 1
seti 1 8 5
mulr 1 5 4
eqrr 4 3 4
addr 4 2 2
addi 2 1 2
addr 1 0 0
addi 5 1 5
gtrr 5 3 4
addr 2 4 2
seti 2 0 2
addi 1 1 1
gtrr 1 3 4
addr 4 2 2
seti 1 1 2
mulr 2 2 2
addi 3 2 3
mulr 3 3 3
mulr 2 3 3
muli 3 11 3
addi 4 7 4
mulr 4 2 4
addi 4 6 4
addr 3 4 3
addr 2 0 2
seti 0 3 2
setr 2 0 4
mulr 4 2 4
addr 2 4 4
mulr 2 4 4
muli 4 14 4
mulr 4 2 4
addr 3 4 3
seti 0 4 0
seti 0 4 2`)


function app (input) {	
	let reg = [1, 0, 0, 0, 0, 0]

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
    let ipReg = 2

    let count = 0
    let first = true
    let regCount = {}

    while (ip < instructions.length) {
        count++
        // console.log(reg)

        if (count % 100 === 0) {
            console.log(regCount, reg)
        }

        reg[ipReg] = ip

        // By logging the number of hits-per-instruction, 
        // I noticed that instructions from 3 and forward looked like a loop.
        if (ip === 3) {
            for (reg[1] = 1; reg[1] <= reg[3]; ++reg[1]) {
                if (reg[3] % reg[1] === 0) { // Short form for inner loop
                    console.log("Adding factor", (reg[3] / reg[1]))
                    reg[0] += (reg[3] / reg[1])
                }
            }    

            ip = 16;
            continue;
        }

        regCount[ip] = (regCount[ip] || 0) + 1

        let instruction = instructions[ip]

        let instr = instruction.match(/(.*) (\d*) (\d.*) (\d.*)/)
        operations[instr[1]](...instr.slice(2).map(Number))

        ip = reg[ipReg]
        ip++
    }

    console.log(reg)
    console.log("Answer:", reg[0])
    return reg[0]
}