class Tester {
	constructor (app) {
		this.app = app
	}

	basic (cases) {
		for (let [key, value] of Object.entries(cases)) {
			test("test", () => {
				expect(this.app(key)).toEqual(value)
			})
		}
	}
}

module.exports = Tester