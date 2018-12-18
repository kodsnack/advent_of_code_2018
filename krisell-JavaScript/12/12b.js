require('../modules/AugmentString')()

const LIMIT = 500000000
app(`initial state: #.####...##..#....#####.##.......##.#..###.#####.###.##.###.###.#...#...##.#.##.#...#..#.##..##.#.##

.##.. => .
..##. => #
.#..# => #
.#.#. => .
..#.. => #
###.. => #
##..# => .
##... => #
#.### => #
.##.# => #
#.... => .
###.# => .
..... => .
.#... => #
....# => .
#.#.. => .
...#. => #
#...# => .
##.#. => .
.#.## => #
..#.# => #
#.#.# => .
.#### => .
##### => .
..### => .
...## => .
#..## => .
#.##. => .
#..#. => #
.###. => #
##.## => #
####. => .`)


function app (input) {	
    let pots = input.lines()[0].slice(15).split('').map((pot, index) => ({ pot, index }))

    let rules = {}
    input.lines().slice(1).forEach((line, index) => {
        let [_, given, then] = line.match(/(.*) => (.*)/)
        rules[given] = then
    })

    let seen = {}
    for (let i = 0; true; ++i) {
        pad()

        if (seen[str(pots)]) { // Loop ("steady state") found
            let result = (LIMIT - i) * 
                pots.filter(pot => pot.pot === '#').length + 
                pots.reduce((acc, pot) => acc + (pot.pot === '#' ? pot.index : 0), 0)

            console.log(result)
            process.exit()
        }

        seen[str(pots)] = i

        pots = pots.map((pot, index) => ({
            pot: rules[LLCRR(pots, pot)] ? rules[LLCRR(pots, pot)] : '.',
            index: pot.index,
        }))
    }

    function LLCRR (pots, pot) {
        let str = pot.pot

        let L = pots.find(p => p.index == pot.index - 1)
        str = (L ? L.pot : '.') + str

        let LL = pots.find(p => p.index == pot.index - 2)
        str = (LL ? LL.pot : '.') + str

        let R = pots.find(p => p.index == pot.index + 1)
        str += (R ? R.pot : '.')

        let RR = pots.find(p => p.index == pot.index + 2)
        str += (RR ? RR.pot : '.')

        return str
    }

    // Pad at the front and back with three dots, which is enough to be "safe"
    function pad () {
        ;[1, -1].forEach(dir => {
            let pot = pots.find(pot => pot.pot === '#')
            let pot_index = pots.findIndex(pot => pot.pot === '#')
            pots = pots.slice(pot_index)

            pots.unshift({ pot: '.', index: pot.index - 1 * dir })
            pots.unshift({ pot: '.', index: pot.index - 2 * dir })
            pots.unshift({ pot: '.', index: pot.index - 3 * dir })

            pots.reverse()
        })
    }

    function str(pots) {
        return pots.map(pot => pot.pot).join('')
    }
}