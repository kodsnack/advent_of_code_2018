require('../modules/AugmentArray')()
require('../modules/AugmentString')()

let recipes = {
    0: 3,
    1: 7,
}

app('580741')

function app (input) {	
    let numberOfRecipes = 2

    let e1 = 0
    let e2 = 1
    let result = ''

    while (true) {
        for (let rec of String(Number(recipes[e1]) + Number(recipes[e2])).split('').map(Number)) {
            recipes[numberOfRecipes++] = rec
            result = result + rec

            if (result.length > input.length) {
                result = result.slice(result.length - String(input).length)    
            }

            if (result == input) {
                console.log(numberOfRecipes - input.length)
                return numberOfRecipes - input.length
            }
        }

        e1 = (e1 + 1 + recipes[e1]) % numberOfRecipes
        e2 = (e2 + 1 + recipes[e2]) % numberOfRecipes
    }
}