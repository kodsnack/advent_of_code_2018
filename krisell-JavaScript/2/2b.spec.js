const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

// const Grid = require('../modules/GridMover')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

const cases = {

}
cases[``] = ''
T.basic(cases)


function app (input) {	
	return input.lines().reduce((winner, line, l_i, lines) => {
		for (let i = l_i + 1; i < lines.length; ++i) {

			let current = line.split('').reduce((acc, char, c) =>
				acc + (lines[i][c] == char ? char : '')
			, '')

			if (current.length > winner.length) {
				return current
			}
		}

		return winner
	}, '')
}