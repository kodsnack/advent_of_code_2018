class Occur {

	static get (str) {
		let occurences = {}

		str.split('').forEach(char => {
			let count = occurences[char] || 0
			occurences[char] = count + 1
		})

		return Object.entries(occurences).map(el => {
			return {
				char: el[0],
				count: el[1],
			}
		}).sort((a, b) => {
			if (a.count === b.count) {
				return a.char < b.char ? -1 : a.char === b.char ? 0 : 1
			}

			return b.count - a.count

		})
	}
}

module.exports = Occur