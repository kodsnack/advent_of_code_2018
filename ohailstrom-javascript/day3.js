const day3 = (input, { width, height, noLog }) => {
  const array = new Array(width)
    .fill(0)
    .map(() => new Array(height).fill(0).map(() => "."));

  const inputArray = input.includes("\n")
    ? input.split("\n")
    : input.split(",");

  const coordinates = inputArray.map(inputString => {
    const [id, , pos, len] = inputString.trim().split(" ");

    return {
      id: id.replace("#", ""),
      pos: pos.replace(":", "").split(","),
      len: len.split("x")
    };
  });
  coordinates.map((value, i) => {
    const x1 = parseInt(value.pos[1]);
    const xDelta = parseInt(value.len[1]);
    const y1 = parseInt(value.pos[0]);
    const yDelta = parseInt(value.len[0]);

    for (let x = x1; x < xDelta + x1; x++) {
      for (let y = y1; y < yDelta + y1; y++) {
        if (array[x][y] === ".") {
          array[x][y] = value.id;
        } else {
          array[x][y] = "X";
        }
      }
    }
  });

  !noLog && console.log("####");
  return array.reduce((acc, value) => {
    !noLog && console.log("");
    return (
      acc +
      value.reduce((acc2, val) => {
        !noLog && process.stdout.write(val);
        return val === "X" ? acc2 + 1 : acc2;
      }, 0)
    );
  }, 0);
};

module.exports = day3;
