const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

const Occur = require('../modules/Occur')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

const cases = {

}
cases[``] = 0
T.basic(cases)


function app (input) {
	let sets = ['DUMMY', 'DUMMY', new Set(), new Set()]

	for (let line of input.lines()) {
		let occurences = Occur.get(line)

		for (let occurence of occurences) {
			if ([2, 3].includes(occurence.count)) {
				sets[occurence.count].add(line)
			}
		}
	}

	return sets[2].size * sets[3].size
}