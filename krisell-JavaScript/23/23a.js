const fs = require('fs')
require('../modules/AugmentArray')()
require('../modules/AugmentString')()

app(fs.readFileSync('./input', 'utf8'))

function app (input) {  
    let bots = []
    let botWithLargest = { r: 0 }

    input.lines().forEach(line => {
        let [_, x, y, z, r] = line.match(/pos=<(.+),(.+),(.+)>, r=(\d+)/).map(Number)
        let bot = { x, y, z, r }

        if (bot.r > botWithLargest.r) {
            botWithLargest = bot
        }

        bots.push(bot)
    })

    let { x, y, z, r } = botWithLargest
    let num = bots.filter(bot => (Math.abs(x - bot.x) + Math.abs(y - bot.y) + Math.abs(z - bot.z) <= r)).length
    console.log(num)
}