function register () {
	Array.prototype.tap = function(fn) {
	  fn(this)
	  return this
	}

	Array.prototype.sum = function () {
		return this.map(Number).reduce((acc, el) => acc + el, 0)
	}

	Array.prototype.trimRows = function () {
	  return this.map(el => el.trim())
	}

	Array.prototype.lfirst = function () {
	  this.sort((a, b) => a - b)

	  return this
	}

	Array.prototype.gfirst = function () {
	  this.sort((a, b) => b - a)

	  return this
	}
}

module.exports = register

