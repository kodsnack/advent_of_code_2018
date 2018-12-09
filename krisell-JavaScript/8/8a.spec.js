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

		let width = 0, sum = 0, children = [] 
		Array(numChildren).fill(0).forEach(_ => {
			children.unshift(parseNode(rest.slice(width)))
			width += children[0].width
		})

		return { 
			sum: rest.slice(width, width + numMeta).sum() + children.map(child => child.sum).sum(), 
			width: 2 + width + numMeta 
		}
	}
}