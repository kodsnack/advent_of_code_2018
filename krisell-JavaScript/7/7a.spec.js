const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

const T = new Tester(app)
const L = new Logger(true)

const cases = {}
T.basic(cases)

function app (input) {	
	let requirements = {}
	let chars = new Set()

	for (let line of input.lines()) {
		let [_, req, step] = line.match(/Step (.) must be finished before step (.) can begin./)
		chars.add(req)
		chars.add(step)
		requirements[step] = (requirements[step] || []).concat([req])
	}

	chars = [...chars.keys()].sort().join('')

	let done = ''

	while (true) {
		if (done.length === chars.length) {
			return done
		}

		done += findNext()

		function findNext() {
			for (let char of chars) {
				if (done.includes(char)) {
					continue
				}

				let reqs = requirements[char] || []
				reqs = reqs.filter(req => !done.includes(req))

				if (reqs.length === 0) {
					return char
				}
			}
		}
	}
}