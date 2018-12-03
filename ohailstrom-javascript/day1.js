const day1 = input => {
  const inputArray = input.includes("\n")
    ? input.split("\n")
    : input.split(",");
  if (inputArray && inputArray.length) {
    const resultsArray = [];
    let firstDuplicate = 0;
    const result = inputArray.reduce((acc, inputVal) => {
      const currentResult = parseInt(inputVal) + acc;
      if (resultsArray[currentResult] && !firstDuplicate) {
        firstDuplicate = currentResult;
      }
      resultsArray.push(currentResult);
      return currentResult;
    }, 0);
    firstDuplicate = findFirstDuplicateFrequency(inputArray, result);
    return { result, firstDuplicate };
  }
  return null;
};

const findFirstDuplicateFrequency = array => {
  let found = false;
  let i = 0;
  let currentResult = 0;
  let resultsObject = { 0: 0 };
  let foundNegative = false;
  let foundPositive = false;
  let firstDuplicate = 0;
  while (
    i < array.length ||
    (array.length <= i && foundNegative && foundPositive && !found && i < 100)
  ) {
    const input = parseInt(array[i % array.length]);
    currentResult = currentResult + input;

    if (typeof resultsObject[currentResult] !== "undefined" && !found) {
      firstDuplicate = currentResult;
      found = true;
    }

    resultsObject[currentResult] = currentResult;

    if (!foundNegative && input < 0) {
      foundNegative = true;
    }

    if (!foundPositive && input >= 0) {
      foundPositive = true;
    }
    i++;
  }
  return firstDuplicate;
};

module.exports = day1;
