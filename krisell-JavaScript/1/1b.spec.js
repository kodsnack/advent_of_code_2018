const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

const T = new Tester(app)
const L = new Logger(true)

const cases = {
    // Add test cases here
}
// cases[`Or here, for multiline`] = 0
T.basic(cases)

function app (input) {  
    let acc = 0
    let visited = {}

    while (true) {
        for (line of input.split(/\n+/).map(Number)) {
            acc += line

            if (visited[acc]) {
                return acc
            }

            visited[acc] = true
        }
    }
}