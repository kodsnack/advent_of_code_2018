class Logger {
	constructor (enabled = true) {
		this.disabled = !enabled
	}

	disable () {
		this.disabled = true
	}

	log (...str) {
		if (this.disabled) {
			return 
		}

		console.log(...str)
	}
}

module.exports = Logger