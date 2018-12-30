const fs = require('fs')
require('../modules/AugmentArray')()
require('../modules/AugmentString')()

app(fs.readFileSync('./input', 'utf8'))

function app (input) {	
    let bots = []
    let bounds = {
        L: { x: 2e10, y: 2e10, z: 2e10 },
        H: { x: -2e10, y: -2e10, z: -2e10 },
    }

	input.lines().forEach(line => {
        let [_, x, y, z, r] = line.match(/pos=<(.+),(.+),(.+)>, r=(\d+)/).map(Number)
        let bot = { x, y, z, r }

        bounds.L.x = Math.min(bounds.L.x, x)
        bounds.L.y = Math.min(bounds.L.y, y)
        bounds.L.z = Math.min(bounds.L.z, z)
        bounds.H.x = Math.max(bounds.H.x, x)
        bounds.H.y = Math.max(bounds.H.y, y)
        bounds.H.z = Math.max(bounds.H.z, z)

        bots.push(bot)
    })


    function integerBetween (min, max) {
        return Math.floor(Math.random() * (max + 1 - min) + min)
    }


    let seen = {}
    let most = 0

    let bestPoint = { x: 0, y: 0, z: 0, num: 0 }

    let allTimeBest = require('./best.json') || { x: 0, y: 0, z: 0, num: 0 }

    while (true) {
        let point = {
            x: integerBetween(bounds.L.x, bounds.H.x),
            y: integerBetween(bounds.L.y, bounds.H.y),
            z: integerBetween(bounds.L.z, bounds.H.z),
        }

        let num = bots.filter(bot => (Math.abs(point.x - bot.x) + Math.abs(point.y - bot.y) + Math.abs(point.z - bot.z) <= bot.r)).length

        if (num < Math.max(allTimeBest.num - 150, 500)) {
            continue
        }

        let visited = {}
        
        let lastNum = bots.filter(bot => (Math.abs(point.x - bot.x) + Math.abs(point.y - bot.y) + Math.abs(point.z - bot.z) <= bot.r)).length

        let STEP = 1000
        for (let i = 0; true; ++i) {
            let num = bots.filter(bot => (Math.abs(point.x - bot.x) + Math.abs(point.y - bot.y) + Math.abs(point.z - bot.z) <= bot.r)).length
            if (num < lastNum || i > 50000) {
                if (STEP == 1 || i > 50000) {
                    console.log("Starting over")
                    bestPoint = { x: 0, y: 0, z: 0, num: 0 }
                    break
                }

                STEP = Math.floor(STEP * 0.8)
            }

            lastNum = num
            let dist = Math.abs(point.x) + Math.abs(point.y) + Math.abs(point.z)

            if (num >= bestPoint.num) {
                if (num > bestPoint.num) {
                    Object.assign(bestPoint, point, { dist, num })
                    console.log(bestPoint)
                } else if (dist < bestPoint.dist) {
                    Object.assign(bestPoint, point, { dist, num })
                }

                if (bestPoint.num >= allTimeBest.num) {
                    if (bestPoint.num > allTimeBest.num) {
                        Object.assign(allTimeBest, bestPoint)
                        fs.writeFileSync('./best.json', JSON.stringify(allTimeBest))
                    } else if (bestPoint.dist < allTimeBest.dist) {
                        Object.assign(allTimeBest, bestPoint)
                        fs.writeFileSync('./best.json', JSON.stringify(allTimeBest))
                    }
                }
            }

            let best = [
                [1, 0, 0], [-1, 0, 0], 
                [1, -1, 0], [-1, -1, 0], 
                [1, 1, 0], [-1, 1, 0], 
                [1, 0, 1], [-1, 0, -1], 
                [1, 1, 1], [-1, 1, -1], 
                [1, -1, 1], [-1, -1, -1], 
            ].map(([dx, dy, dz]) => {
                let val = bots.map(bot => {
                    let dist = Math.abs(point.x + dx * STEP - bot.x) + Math.abs(point.y + dy * STEP - bot.y) + Math.abs(point.z + dz * STEP - bot.z)
                    return dist > bot.r ? dist : 0
                }).sum()

                return { dx, dy, dz, val }
            })

            best.sort((a, b) => a.val - b.val)

            let index = 0
            while (visited[`${point.x + STEP * best[index].dx},${point.y + STEP * best[index].dy},${point.z + STEP * best[index].dz}`]) {
                index++

                if (index > 5) {
                    index = 1
                    process.exit()
                }
            }

            point.x += STEP * best[index].dx
            point.y += STEP * best[index].dy
            point.z += STEP * best[index].dz

            visited[`${point.x},${point.y},${point.z}`] = true
        }
    }
}