require('../modules/AugmentString')()
const memoize = require('fast-memoize')
const Heap = require('qheap');

app(`depth: 4080
target: 14,785`)

function app (input) {  
    const type = (x, y) => (x < 0 || y < 0) ? -1 : erosion(x, y) % 3
    const erosion = (x, y) => (geoM(x, y) + depth) % 20183

    const allows = {
        0: { 'C': true, 'T': true },
        1: { '': true, 'C': true },
        2: { '': true, 'T': true },
    }

    let geoM = memoize(geoIndex)

    let depth = input.lines()[0].match(/depth: (\d+)/).map(Number)[1]
    let target = input.lines()[1].match(/target: (.+)/)[1]
    let [tx, ty] = target.split(',').map(Number)

    let risk = 0
    for (let y = 0; y <= ty; ++y) {
        for (let x = 0; x <= tx; ++x) {
            risk += type(x, y)
        }
    }

    console.log("a)", risk)

    let visits = new Map()

    let pq = new Heap({
        comparBefore: (a, b) => a.time < b.time
    })

    pq.insert({ x: 0, y: 0, time: 0, tool: 'T' })

    while (pq.length) {
        let { x, y, time, tool } = pq.dequeue()

        let key = `${x},${y},${tool}`
        if (visits.get(key) <= time) {
            continue // Been here faster
        } 

        visits.set(key, time)

        if (x === tx && y === ty && tool === 'T') {
            console.log("b)", time)
            process.exit()
        }

        time += 1
        for (let typeIndex of [0, 1, 2]) {
            if (type(x + 1, y) === typeIndex && allows[typeIndex][tool]) {
                pq.insert(({ time, x: x + 1, y, tool }))            
            }

            if (type(x, y + 1) === typeIndex && allows[typeIndex][tool]) {
                pq.insert(({ time, x, y: y + 1, tool }))            
            }

            if (type(x, y - 1) === typeIndex && allows[typeIndex][tool]) {
                pq.insert(({ time, x, y: y - 1, tool }))            
            }

            if (type(x - 1, y) === typeIndex && allows[typeIndex][tool]) {
                pq.insert(({ time, x: x - 1, y, tool}))            
            }
        }

        for (let change of ['T', 'C', '']) {
            if (change === tool || !allows[type(x, y)][change]) {
                continue
            }

            pq.insert({ time: time + 6, x, y, tool: change })
        }
    }

    function geoIndex (x, y) {
        if (x == tx && y == ty) {
            return 0
        }

        if (x == 0 && y == 0) {
            return 0
        }

        if (y == 0) {
            return x * 16807
        }

        if (x == 0) {
            return y * 48271
        }

        return erosion(x - 1, y) * erosion(x, y - 1)
    }
}   