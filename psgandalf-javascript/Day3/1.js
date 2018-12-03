const fs = require('fs')
const data = fs.readFileSync('input.txt', 'utf8')
let array = data.split('\n');
let sum = 0
let coordinates = {}
let highy = 0
let highx = 0

array.map((line) => {
    const a = line.split('#')
    const b = a[1].split('@')
    const c = b[1].split(',')
    const d = c[1].split(':')
    const e = d[1].split('x')
    //console.log(a, b, c, d, e)
    const nr = parseInt(b[0].trim())
    const x = parseInt(c[0].trim())
    const y = parseInt(d[0].trim())
    const width = parseInt(e[0].trim())
    const height = parseInt(e[1].trim())
    //console.log(nr, x, y, width, height)
    for (let x1 = x; x1 < x + width; x1++) {
        for (let y1 = y; y1 < y + height; y1++) {
            if (coordinates[x1] == undefined) {
                coordinates[x1] = {}
            }
            if (x1 > highx) {
                highx = x1
            }
            if (coordinates[x1][y1] == undefined) {
                coordinates[x1][y1] = []
                if (y1 > highy) {
                    highy = y1
                }
            }
            coordinates[x1][y1].push(nr)
        }
    }

});
for (let x1 = 0; x1 <= highx; x1++) {
    for (let y1 = 0; y1 <= highy; y1++) {
        if (x1 in coordinates) {
            if (y1 in coordinates[x1]) {
                if (coordinates[x1][y1].length > 1) {
                    sum += 1
                }
            }
        }
    }
}

console.log(sum)