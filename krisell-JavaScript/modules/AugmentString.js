function register () {
	String.prototype.lines = function () {
	  return this.split(/\n+/g)
	}	
}

module.exports = register

