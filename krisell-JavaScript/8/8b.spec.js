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
cases[``] = 0
T.basic(cases)

function app (input) {	
	return parseNode(input.split(' ').map(Number)).sum

	function parseNode(data) {
		let [numChildren, numMeta, ...rest] = data
		
		let width = 0
		let childValues = {}
		for (let i = 1; i <= numChildren; ++i) {
			let { sum, width: childWidth } = parseNode(rest.slice(width))
			childValues[i] = sum	
			width += childWidth
		}

		if (numChildren == 0) {
			return {
				sum: rest.slice(width, width + numMeta).sum(),
				width: 2 + width + numMeta,
			}
		}

		return {
			sum: Array(numMeta).fill(0).map((_, index) => childValues[rest[width + index]] || 0).sum(),
			width: 2 + width + numMeta
		}
	}
}