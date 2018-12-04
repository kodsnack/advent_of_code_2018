const calculateDuplicates = input => {
  const array = input.split("");
  let two = 0;
  let three = 0;
  const returnVal = array.reduce((acc, letter) => {
    if (acc[letter]) {
      return {
        ...acc,
        [letter]: acc[letter] + 1
      };
    } else {
      return {
        ...acc,
        [letter]: 1
      };
    }
  }, {});
  Object.values(returnVal).map(val => {
    if (val === 3) three = 1;
    if (val === 2) two = 1;
    return val;
  });

  return { two, three };
};

const getChecksum = input => {
  const inputArray = input.includes("\n")
    ? input.split("\n")
    : input.split(",");
  let twos = 0;
  let threes = 0;
  const duplicates = inputArray.reduce((acc, str) => {
    const { two, three } = calculateDuplicates(str);
    twos = twos + two;
    threes = threes + three;
  }, {});
  return twos * threes;
};

const getMostSimiliar = input => {
  const inputArray = input.includes("\n")
    ? input.split("\n")
    : input.split(",");
  const diff = inputArray.reduce((acc, val) => {
    const trimmedStr = val.trim();
    const trimmedStrSplit = trimmedStr.split("");
    const test = inputArray.reduce((accum, str) => {
      if (str !== val) {
        const inputSplit = str.trim().split("");
        const diff = inputSplit.reduce((acc, cur, idx) => {
          const element = trimmedStrSplit[idx];
          const sameChar = inputSplit[idx] === element;
          return acc + (sameChar ? element : "");
        }, "");
        if (trimmedStr.length - diff.length === 1) {
          return diff;
        }
      }
      return accum;
    }, "");
    if (trimmedStr.length - test.length === 1) {
      return test;
    }
    return acc;
  }, "");

  return diff;
};

module.exports = { calculateDuplicates, getChecksum, getMostSimiliar };
