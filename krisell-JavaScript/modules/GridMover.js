const _ = require('lodash')

class GridMover {
	constructor (direction, position) {
		this.direction = direction || { NS: 1, EW: 0 }
		this.position = position || { NS: 0, EW: 0 }

		this.visited = new Map()
		this.firstCollision = true
	}

	// Turning R (right) or L (left)
	turn (dir) {
		let { NS, EW } = this.direction
		this.direction = {
			NS: (dir === 'R' ? -1 : 1) * EW,
			EW: (dir === 'R' ? 1 : -1) * NS
		}
	}

	// ^ v < >
	goDir (dir) {
		let { NS, EW } = this.direction
		this.direction = {
			NS: (dir === '^' ? 1 : dir === 'v' ? -1 : 0),
			EW: (dir === '>' ? 1 : dir === '<' ? -1 : 0),
		}

		this.move(1)
	}

	// Moving steps steps in the current direction
	move (steps) {
		_.range(0, steps).forEach(step => {
			this.position.NS += this.direction.NS
			this.position.EW += this.direction.EW

			let key = JSON.stringify(this.position)
			if (this.visited.get(key) && this.onCollisionCallback) {
				this.onCollisionCallback(this.position)
			}

			this.visited.set(JSON.stringify(this.position), 1)
		})
	}

	visit () {
		this.visited.set(JSON.stringify(this.position), 1)
	}

	getPosition () {
		return this.position
	}

	get md () {
		return this.distanceToOrigin()
	}

	distanceToOrigin () {
		return Math.abs(this.position.NS) + Math.abs(this.position.EW)
	}

	onFirstCollision (cb) {
		this.onCollisionCallback = () => {
			if (this.firstCollision) {
				cb(this.position)
			}
		
			this.firstCollision = false	
		}
	}
}

module.exports = GridMover