const part1 = input =>
  input
    .split("")
    .reduce(
      (acc, char) =>
        Math.abs(
          acc.substring(acc.length - 1).charCodeAt() - char.charCodeAt()
        ) === 32
          ? acc.slice(0, acc.length - 1)
          : acc + char,
      ""
    );

const part2 = input =>
  Math.min(
    ...input
      .split("")
      .reduce((obj, val) => {
        if (!obj.includes(val.toLowerCase())) obj.push(val.toLowerCase());
        return obj;
      }, [])
      .map(letter =>
        input
          .split("")
          .reduce(
            (acc, char) => (char.toLowerCase() === letter ? acc : acc + char),
            ""
          )
      )
      .map(part1)
      .map(str => str.length)
  );

module.exports = { part1, part2 };
