const fs = require('fs')
const data = fs.readFileSync('input.txt', 'utf8')
let array = data.split('\n');
let sum = 0
let answer = ""
for (var a = 0; a < array.length; a++) {
    for (var i = 0; i < array.length; i++) {
        if (a != i) {
            let diffs = 0
            let pos = -1
            for (var j = 0; j < array[0].length; j++) {
                if ((array[a][j]) != (array[i][j])) {
                    diffs += 1
                    pos = j
                }
            }
            if (diffs === 1) {
                for (var k = 0; k < array[0].length; k++) {
                    if (k != pos) {
                        answer += array[a][k]
                    }
                }
                console.log(answer)
                process.exit(-1)
            }
        }
    }
}
