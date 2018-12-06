const Logger = require('../modules/Logger')
const Tester = require('../modules/Tester')

require('../modules/AugmentArray')()
require('../modules/AugmentString')()

const Occur = require('../modules/Occur')
// let g = new Grid()

const T = new Tester(app)
const L = new Logger(true)

const cases = {

}
cases[``] = 0
T.basic(cases)

function app (input) {  
    let guard = 0
    let schedule = {}
    let dates = []

    for (line of input.lines().sort()) {
        let [timestamp, action] = line.split(']').map(el => el.trim())
        let [date, time] = timestamp.slice(1).split(' ')
        let minutes = time.split(':')[1]
        dates[date] = true

        if (action.match(/Guard #(\d+) begins shift/)) {
            guard = action.match(/Guard #(\d+) begins shift/)[1]
            schedule[guard] = (schedule[guard] || [])
        }

        if (action.match(/falls asleep/)) {
            schedule[guard].push({ date, type: 'sleeping', minutes })
        }

        if (action.match(/wakes up/)) {
            schedule[guard].push({ date,  type: 'awake', minutes })
        }
    }

    let status = {}
    let sleeptimes = {}
    Object.keys(dates).forEach(date => {
        for (let i = 0; i < 60; ++i) {
            for (let [guard, entries] of Object.entries(schedule)) {
                for (entry of entries.filter(entry => entry.date == date)) {
                    if (entry.minutes == i) {
                        status[guard] = entry.type
                    }
                }

                if (status[guard] == 'sleeping') {
                    sleeptimes[guard] = [...(sleeptimes[guard] || []), i]
                }
            }
        }
    })

    let winner = Object.entries(sleeptimes).map((sleeps) => ({
        guard: sleeps[0],
        sum: sleeps[1].sum(),
        minute: Occur.best(sleeps[1]).entry,
    })).sort((a, b) => b.sum - a.sum)[0]

    return winner.minute * winner.guard
}