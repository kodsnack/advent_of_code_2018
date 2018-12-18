const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')
const fs = require('fs')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

let input = fs.readFileSync('./real', 'utf8')

const T = new Tester(app)
const L = new Logger(true)

app(input)

function app (input) {	
    let track = {}
    let carts = []

    input.lines().forEach((line, r) => {
        line.chars().forEach((char, c) => {
            let cartDirections = {
                '<': { replacement: '-', dir: 'L' },
                '>': { replacement: '-', dir: 'R' },
                '^': { replacement: '|', dir: 'U' },
                'v': { replacement: '|', dir: 'D' },
            }

            if (!cartDirections[char]) {
                track[`${c},${r}`] = char
                return    
            }

            carts.push({ c, r, dir: cartDirections[char].dir, next: 'L' })
            track[`${c},${r}`] = cartDirections[char].replacement
        })
    })

    let moves = {
        '-': {
            L: 'L',
            R: 'R'
        },
        '|': {
            U: 'U',
            D: 'D'
        },
        '/': {
            U: 'R',
            L: 'D',
            R: 'U',
            D: 'L',
        },
        '\\': {
            R: 'D',
            D: 'R',
            L: 'U',
            U: 'L',
        },
        '+': {
            L: 'A',
            R: 'A',
            U: 'A',
            D: 'A',
        }
    }

    let turningMapper = {
        L: {
            D: 'R',
            U: 'L',
            L: 'D',
            R: 'U',
            N: 'S',
        },
        R: {
            D: 'L',
            U: 'R',
            L: 'U',
            R: 'D',
            N: 'L',
        },
        S: {
            N: 'R'
        }
    }

    while (true) {
        carts.sort((a, b) => (a.r === b.r) ? a.c - b.c : a.r - b.r)

        for (let [index, cart] of carts.entries()) {
            for (let [index, cart] of carts.entries()) {
                if (carts.find(c => cart !== c && c.r === cart.r && c.c === cart.c)) {
                    console.log(`${carts[0].c},${carts[0].r}`)
                    return `${carts[0].c},${carts[0].r}`
                }
            }

            let moveResult = {
                U: { r: -1, c: 0 },
                D: { r: 1, c: 0 },
                L: { r: 0, c: -1 },
                R: { r: 0, c: 1 },
            }

            cart.r += moveResult[cart.dir].r
            cart.c += moveResult[cart.dir].c
            let dir = moves[track[`${cart.c},${cart.r}`]][cart.dir]

            if (dir !== 'A') {
                cart.dir = dir
                continue
            }

            cart.dir = turningMapper[cart.next][cart.dir] || cart.dir
            cart.next = turningMapper[cart.next].N
        }
    }
}