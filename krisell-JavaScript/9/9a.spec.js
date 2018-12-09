const Tester = require('../modules/Tester')

const T = new Tester(app)

const cases = {}

T.basic(cases)

function app (input) {  
    let [_, players, last] = input.match(/(.*) players; last marble is worth (.*) points/)

    let scores = {}

    let startMarble = { value: 0 }
    startMarble.next = startMarble
    startMarble.prev = startMarble

    let marble = startMarble

    for (let i = 1; true; ++i) {
        if (i % 23 === 0) {
            marble = marble.prev.prev.prev.prev.prev.prev.prev

            marble.next.prev = marble.prev
            marble.prev.next = marble.next

            scores[i % players + 1] = (scores[i % players + 1] || 0) + i + marble.value
            marble = marble.next
            continue
        }

        if (i >= last)
        {
            return Object.values(scores).sort((a, b) => b - a)[0]
        }

        let newMarble = {
            value: i,
            next: marble.next.next,
            prev: marble.next
        }

        marble.next.next = newMarble
        newMarble.next.prev = newMarble

        marble = newMarble
    }
}