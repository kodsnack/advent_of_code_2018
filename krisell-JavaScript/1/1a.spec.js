const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

const T = new Tester(app)
const L = new Logger(true)

require('../modules/AugmentArray')()

const cases = {
    // Add test cases here
}
// cases[`Or here, for multiline`] = 0
T.basic(cases)

function app (input) {  
    return input.split(/\n+/).map(Number).sum()
}