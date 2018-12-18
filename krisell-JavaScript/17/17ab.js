const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')
const fs = require('fs')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

// const Grid = require('../modules/GridMover')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

let map = {}
let branches = []

const cases = {

}

let input = fs.readFileSync('./input', 'utf8')

app(input)
// T.basic(cases)

function app (input) {  
    let oob = {}

    let yBounds = {
        low: 2e10,
        high: -2e10,
    }

    let xBounds = {
        low: 2e10,
        high: -2e10,
    }

    input.lines().forEach(line => {
        let match = line.match(/x=(.*), y=(.*)\.\.(.*)/)

        if (match) {
            let [_, x, y_1, y_2] = match.map(Number)

            if (x < xBounds.low) {
                xBounds.low = x
            }

            if (x > xBounds.high) {
                xBounds.high = x
            }

            for (let r = y_1; r <= y_2; ++r) {
                map[`${x},${r}`] = '#'

                if (r < yBounds.low) {
                    yBounds.low = r
                }

                if (r > yBounds.high) {
                    yBounds.high = r
                }
            }
        }
        
        match = line.match(/y=(.*), x=(.*)\.\.(.*)/)

        if (match) {
            let [__, y, x_1, x_2] = match.map(Number)

            if (y < yBounds.low) {
                yBounds.low = y
            }

            if (y > yBounds.high) {
                yBounds.high = y
            }

            for (let c = x_1; c <= x_2; ++c) {
                map[`${c},${y}`] = '#'

                if (c < xBounds.low) {
                    xBounds.low = c
                }

                if (c > xBounds.high) {
                    xBounds.high = c
                }
            }
        }

    })

    console.log(xBounds, yBounds)
    let visited = {}
    let spring = { x: 500, y: 0 }
    
    let waters = {}
    let index = 0
    branches = []
    let highestY = 0
    
    let ts

    for (let i = 0; true; ++i) {
        ts = {}

        if (i > 0 && i % 300 === 0) {
            console.log("a)", Object.values(visited).length)
            console.log("b)", Object.values(visited).filter(water => water.moving === false).length)

            if (Object.values(visited).length === 37649) {
                // print()
                process.exit()
            }
        }

        branches = [spring]

        while (branches.length) {
            branches.sort((a, b) => {
                return b.y - a.y
            })


            let water = branches.pop()

            let cnhildBranches = move(water)
            if (cnhildBranches.length) {
                branches.push(...cnhildBranches)
            }

            if (water.y >= yBounds.low && water.y <= yBounds.high) {
                visited[`${water.x},${water.y}`] = water
            }
        }
    }

    print()

    console.log(Object.values(visited).length)
    console.log(Object.values(visited).filter(water => water.moving = false).length)
        

    function isFree (x, y) {
        if (visited[`${x},${y}`]) {
            return map[`${x},${y}`] !== '#' && visited[`${x},${y}`].moving
        }

        return map[`${x},${y}`] !== '#'
    }

    function move (water) {

        let returns = []

        if (water.y < 0 || water.y > yBounds.high - 1) {
            oob[`${water.x},${water.y}`] = true
            return false
        }

        if (isFree(water.x, water.y + 1)) {
            if (water.branchPoint) {
                ts[`${water.branchPoint.x},${water.branchPoint.y}`] = water.y + 1
            }

            let point = {
                x: water.x,
                y: water.y + 1,
                dir: 'D',
                moving: true,
            }

            if (branches.length > 0 && visited[`${point.x},${point.y}`]) {
                if (Math.random() < 0.9) {
                    return [point]    
                }

                return []
            }

            return [point]    
        }

        if (isFree(water.x - 1, water.y) && water.dir !== 'R') {
             returns.push({
                x: water.x - 1,
                y: water.y,
                dir: 'L',
                moving: true,
                branchPoint: water.branchPoint,
            })
        }    

        if (isFree(water.x + 1, water.y) && water.dir !== 'L') {
            returns.push({
                x: water.x + 1,
                y: water.y,
                dir: 'R',
                moving: true,
                branchPoint: water.branchPoint,
            })
        }

        if (returns.length === 2) {
            returns.forEach(w => {
                w.branchPoint = { x: water.x, y: water.y }
            })
        }

        if (returns.length === 0) {
            let left = visited[`${water.x - 1},${water.y}`] || {}
            let right = visited[`${water.x + 1},${water.y}`] || {}
            if (left.moving === false || right.moving === false) {
                water.moving = false
            } else if (water.branchPoint) {
                let { x, y } = water.branchPoint
                if (ts[`${x},${y}`]) {
                    water.moving = (ts[`${x},${y}`] > water.y)
                } else {
                    water.moving = true
                    ts[`${x},${y}`] = water.y
                }    
            } else {
                water.moving = false
            }
        }

        return returns
    }

    function print () {
        process.stdout.write('\n')
        process.stdout.write('\n')
        for (let y = yBounds.low - 1; y <= yBounds.high + 1; ++y) {
            for (let x = xBounds.low - 1; x <= xBounds.high + 1; ++x) {

                let piece = map[`${x},${y}`] || '.'

                found = branches.find(branch => branch.x === x && branch.y === y) 
                if (found) {
                    process.stdout.write('X')    
                } else if (visited[`${x},${y}`]) {
                    process.stdout.write(visited[`${x},${y}`].moving ? '|' : '~')    
                } else {
                    process.stdout.write(piece)
                }
            }

            process.stdout.write('\n')
        }
    }
}