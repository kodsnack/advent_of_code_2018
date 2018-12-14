function char_count(str, letter) {
    var letter_Count = 0;
    for (var position = 0; position < str.length; position++) {
        if (str.charAt(position) == letter) {
            letter_Count += 1;
        }
    }
    return letter_Count;
}

const fs = require('fs')
const data = fs.readFileSync('input.txt', 'utf8')
let array = data.split('\n');
let sum = 0
let sum_twos = 0
let sum_trees = 0

array.map((line) => {
    found_two = false
    found_tree = false
    for (var i = 0; i < line.length; i++) {
        if (char_count(line, line.charAt(i)) === 2) {
            found_two = true
        }
        if (char_count(line, line.charAt(i)) === 3) {
            found_tree = true
        }
    }
    if (found_two) {
        sum_twos += 1
    }
    if (found_tree) {
        sum_trees += 1
    }
});
console.log(sum_twos * sum_trees)