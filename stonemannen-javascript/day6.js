const fs = require("fs");
var input = fs.readFileSync("input6.txt", "utf8");
input = input.split("\n");

var highestX = 0
var highestY = 0

var coordinates = []

for(var i = 0; i < input.length; i++){
    var inn = input[i].split(", ")
    x = parseInt(inn[0])
    y = parseInt(inn[1])
    coordinates.push({x: x, y: y})
    if(highestX < x){
        highestX = x
    }
    if(highestY < y){
        highestY = y
    }
}

var grid = makeArray(highestX+2, highestY+2, -1)


for(var i = 0; i < grid.length;i++){
    for(var j = 0; j < grid[i].length; j++){
        
        var shortest = 100000
        for(var k = 0; k < coordinates.length; k++){
            var negX = (i-coordinates[k].x)
            var distanceX = -negX>0 ? -negX : negX;
            var negY = (j-coordinates[k].y)
            var distanceY = -negY>0 ? -negY : negY;
            var distance = distanceX + distanceY
            if(shortest > distance){
                shortest = distance
                grid[i][j] = k
            }else if(shortest == distance){
                grid[i][j] = "."
            }
        }      
    }
}

/*for(var i = 0; i < grid.length; i++){
    if(i == 0||i == grid.length-1){
        for(var j = 0; j<grid[i].length; j++){
            grid[i][j] = -1
        }
    }else{
        for(var j = 0; j<grid[i].length; j+=grid[i].length-1){
            grid[i][j] = -1
        }
    }
}*/

var finite = []
var infinite = []
for(var i= 0; i < coordinates.length; i++){
    var fin = 1
    for(var j = coordinates[i].x; j < grid.length-1; j++){
        if(grid[j][coordinates[i].y] != i && grid[j][coordinates[i].y] != -1){
            fin++
            break;
        }
    }
    for(var j = coordinates[i].x; j > 0; j--){
        if(grid[j][coordinates[i].y] != i && grid[j][coordinates[i].y] != -1){
            fin++
            break;
        }
    }
    for(var j = coordinates[i].y; j < grid.length[0]-1; j++){
        if(grid[j][coordinates[i].x] != i && grid[j][coordinates[i].x] != -1){
            fin++
            break;
        }
    }
    for(var j = coordinates[i].y; j > 0; j--){
        if(grid[j][coordinates[i].x] != i && grid[j][coordinates[i].x] != -1){
            fin++
            break;
        }
    }
    if(fin == 4){
        finite.push({id: i, count: 0})
    }
}

function makeArray(w, h, val) {
    var arr = [];
    for(i = 0; i < h; i++) {
        arr[i] = [];
        for(j = 0; j < w; j++) {
            arr[i][j] = val;
        }
    }
    return arr;
}

for(var i = 0; i < grid.length; i++){
    for(var j = 0; j < grid.length; j++){
        for(var k = 0; k < finite.length; k++){
            if(grid[i][j] == finite[k].id){
                finite[k].count++
            }
        }
    }
}

var longest = 0;
for(var i = 0; i < finite.length; i++){
    if(finite[i].count > longest&&finite[i].count<5000){
        longest = finite[i].count
    }
}

console.log(longest)