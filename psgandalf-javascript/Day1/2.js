const fs = require('fs')
const data = fs.readFileSync('input.txt', 'utf8')
let array = data.split('\n');
let sum = 0
let sums = [0]
let found = false
let i

while (!found) {
    for (i = 0; i < array.length; i++) {
        sum += parseInt(array[i])
        if (sums.indexOf(sum) != -1) {
            found = true
            break;
        }
        sums.push(sum)
    }
}

console.log(sum)

