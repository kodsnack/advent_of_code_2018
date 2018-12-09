const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()


const T = new Tester(app)
const L = new Logger(true)

const cases = {}

T.basic(cases)

function app (input) {  
    let requirements = {}
    let workers = Array(5).fill(0)
    let chars = new Set()

    for (let line of input.lines()) {
        let [_, req, step] = line.match(/Step (.) must be finished before step (.) can begin./)
        chars.add(req)
        chars.add(step)
        requirements[step] = (requirements[step] || []).concat([req])
    }

    chars = [...chars.keys()].join('')

    let work = []
    let clock = 0
    let done = []

    while (true) {
        let ready = work.filter(el => Number(el.startTime) + el.endTime <= clock)
        work = work.filter(el => !ready.includes(el))
        done.push(...ready.map(el => el.char))

        if (done.length === chars.length) {
            return clock
        }

        let candidates = []

        for (let char of chars) {
            if (done.includes(char) || work.find(worker => worker.char == char)) {
                continue
            }

            let reqs = requirements[char] || []
            reqs = reqs.filter(req => !done.includes(req))

            if (reqs.length === 0) {
                candidates.push(char)
            }
        }

        if (candidates.length) {
            workers.forEach((_, index) => {
                if (!work.find(worker => worker.index === index)) {
                    let char = candidates.sort().shift()    
                    if (char) {
                        work.push({ index, char, startTime: clock, endTime: 60 + parseInt(char, 36) - 9 })  
                    }
                }
            })
        }

        clock++
    }
}