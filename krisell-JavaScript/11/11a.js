
/**
 * I originally went for the brute force approach, which workes but has a runtime of
 * a few minutes. I the came across the concept here: https://en.wikipedia.org/wiki/Summed-area_table
 * and decided to try to implement that.
 */

let sumTable = {}

function setI (c, r, val) {
    return sumTable[`${c},${r}`] = val
}

function getI (c, r) {
    return sumTable[`${c},${r}`] || 0
}

function power (x, y, serial) {
    let rack = Number(x) + 10
    let power = Math.floor((rack * ((rack * y) + serial)) / 100) % 10 - 5
    return power
}

app(7511)

function app (serial) {  
    let winner = { val: -2e10, coord: '' }

    // Build Summed area table
    for (let r = 1; r <= 300; ++r) {
        for (let c = 1; c <= 300; ++c) {
            setI(c, r, power(c, r, serial) + getI(c, r - 1) + getI(c - 1, r) - getI(c - 1, r - 1))
        }
    }

    for (let r = 1; r <= 300; ++r) {
        for (let c = 1; c <= 300; ++c) {

            for (let size = 3; size <= 3; ++size) {
                if (r + size > 301 || c + size > 301) {
                    continue
                }

                let sum = getI(c + size - 1, r + size - 1) + getI(c - 1, r - 1) - getI(c + size - 1, r - 1) -  getI(c - 1, r + size - 1)

                if (sum > winner.val) {
                    winner.val = sum
                    winner.coord = `${c},${r},${size}`
                }
            }
        }
    }

    console.log(winner)
    return winner
}