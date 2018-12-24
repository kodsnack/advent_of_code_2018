require('../modules/AugmentArray')()
require('../modules/AugmentString')()

app(
`Immune System:
76 units each with 3032 hit points with an attack that does 334 radiation damage at initiative 7
4749 units each with 8117 hit points with an attack that does 16 bludgeoning damage at initiative 16
4044 units each with 1287 hit points (immune to radiation, fire) with an attack that does 2 fire damage at initiative 20
1130 units each with 11883 hit points (weak to radiation) with an attack that does 78 radiation damage at initiative 14
1698 units each with 2171 hit points (weak to slashing, fire) with an attack that does 11 bludgeoning damage at initiative 12
527 units each with 1485 hit points with an attack that does 26 bludgeoning damage at initiative 17
2415 units each with 4291 hit points (immune to radiation) with an attack that does 17 cold damage at initiative 5
3266 units each with 6166 hit points (immune to cold, slashing; weak to radiation) with an attack that does 17 bludgeoning damage at initiative 18
34 units each with 8390 hit points (immune to cold, fire, slashing) with an attack that does 2311 cold damage at initiative 10
3592 units each with 5129 hit points (immune to cold, fire; weak to radiation) with an attack that does 14 radiation damage at initiative 11
Infection:
3748 units each with 11022 hit points (weak to bludgeoning) with an attack that does 4 bludgeoning damage at initiative 6
2026 units each with 11288 hit points (weak to fire, slashing) with an attack that does 10 slashing damage at initiative 13
4076 units each with 23997 hit points (immune to cold) with an attack that does 11 bludgeoning damage at initiative 19
4068 units each with 40237 hit points (immune to cold; weak to slashing) with an attack that does 18 slashing damage at initiative 4
3758 units each with 16737 hit points (weak to slashing) with an attack that does 6 radiation damage at initiative 2
1184 units each with 36234 hit points (weak to bludgeoning, fire; immune to cold) with an attack that does 60 radiation damage at initiative 1
1297 units each with 36710 hit points (immune to cold) with an attack that does 47 fire damage at initiative 3
781 units each with 18035 hit points (immune to bludgeoning, slashing) with an attack that does 36 fire damage at initiative 15
1491 units each with 46329 hit points (immune to slashing, bludgeoning) with an attack that does 56 fire damage at initiative 8
1267 units each with 34832 hit points (immune to cold) with an attack that does 49 radiation damage at initiative 9`)

function app (lines) {	
    const ep = group => group.units * group.ap

    function damage (attacker, target) {
        let damage = ep(attacker)
        if (target.weakness.includes(attacker.at)) {
            damage *= 2
        }

        return target.immunities.includes(attacker.at) ? 0 : damage
    }

    let boost = -1
    while (true) {
        console.log("Trying with a boost-value of", ++boost)
        let groups = []
        let type = ''
        lines.lines().forEach((line, index) => {
            if (line.match(/(.*):/)) {
                type = line.match(/(.*):/)[1].split(' ')[0].toLowerCase()
                return
            }

            let weakness = []
            let immunities = []

            let matches = line.match(/(\d+) units each with (\d+) hit points/).map(Number)
            let [_, units, hp] = matches

            matches = line.match(/with an attack that does (\d+) (.*) damage at initiative (\d+)/)
            let [__, ap, at, ini] = matches.map((_, index) => [1, 3].includes(index) ? +_ : _)
            ap += (type === 'immune' ? boost : 0)

            matches = line.match(/weak to (.*?)[;\)]/)
            if (matches) {
                weakness = matches[1].split(';')[0].trim().split(', ')
            }

            matches = line.match(/immune to (.*?)\)/)
            if (matches) {
                immunities = matches[1].split(';')[0].trim().split(', ')
            }

            groups.push({ type, units, hp, ap, at, ini, weakness, immunities })
        })

        let totalUnits = 0
        while (true) {
            // If nothing changed from last iteration, the battle is stuck
            // (since no-one can deal enough damage to kill even one enemy unit).
            if (groups.map(group => group.units).sum() === totalUnits) {
                break;
            }

            totalUnits = groups.map(group => group.units).sum()

            if (groups.filter(group => group.type === 'infection').length === groups.length || groups.filter(group => group.type === 'infection').length === 0) {
                if (groups[0].type === 'immune') {
                    console.log(groups.map(group => group.units).sum())
                    process.exit()
                }
              
                break
            }

            groups.sort((a, b) => ep(a) === ep(b) ? b.ini - a.ini : ep(b) - ep(a))

            for (let group of groups) {
                group.attacker = null
            }

            for (let group of groups) {
                let targets = groups.filter(g => g.type !== group.type)

                targets.sort((a, b) => {
                    if (damage(group, a) === damage(group, b)) {
                        if (ep(a) === ep(b)) {
                            return b.ini - a.ini
                        }

                        return ep(b) - ep(a)
                    }

                    return damage(group, b) - damage(group, a)
                })

                targets = targets.filter(target => damage(group, target) > 0 && target.attacker === null)


                group.target = null
                if (targets.length) {
                    group.target = targets[0]
                    targets[0].attacker = group
                }
            }

            groups.sort((a, b) => b.ini - a.ini)

            for (let group of groups) {
                if (group.units === 0) {
                    continue
                }

                if (group.target) {
                    group.target.units = Math.max(0, group.target.units - Math.floor(damage(group, group.target ) / group.target.hp))
                }
            }

            groups = groups.filter(group => group.units > 0)
        }
    }
}