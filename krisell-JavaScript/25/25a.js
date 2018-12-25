require('../modules/AugmentString')()

function app (input) {	
    let index = 0
    let stars = []

	input.lines().forEach(line => {
        let [_, a, b, c, d] = line.match(/(.*),(.*),(.*),(.*)/).map(Number)
        let adjacent = stars.filter(star => Math.abs(star.a - a) + Math.abs(star.b - b) + Math.abs(star.c - c) + Math.abs(star.d - d) <= 3)

        constellation = index++

        if (adjacent.length) {
            constellation = adjacent[0].constellation

            adjacent.forEach(adj => {
                stars.filter(star => star.constellation === adj.constellation).forEach(star => star.constellation = constellation)
            })
        }

        stars.push({ constellation, a, b, c, d })
    })

    let size = new Set(stars.map(star => star.constellation)).size
    console.log(size)
    return size
}