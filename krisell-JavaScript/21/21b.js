let founds = {}
let r1 = 0, r2 = 0, r3 = 0, r4 = 0, r5 = 0 

while (true) {
    r3 = r4 | 65536
    r4 = 3730679
    
    while (true) {
        r5 = r3 & 255
        r4 += r5
        r4 = r4 & 16777215
        r4 *= 65899
        r4 = r4 & 16777215

        if (256 > r3) {
            // Jumps to where r0 is compared to r4, and exits if they are

            if (!founds[r4]) {
                founds[r4] = true
                console.log(r4)
                
            }

            // Hitting instruction 28 and not having r0 === r4
            // goes back to the start, but not resetting r4 to 0
            break
        }

        r5 = 0

        while (r5 * 256 < r3) {
            r5++
        }

        r3 = r5
    }
}

