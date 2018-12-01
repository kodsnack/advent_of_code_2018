const fs = require('fs')
const data = fs.readFileSync('input.txt', 'utf8')
let array = data.split('\n');
let sum = 0
array.map((val) => {
    sum += parseInt(val, 10)
});
console.log(sum)