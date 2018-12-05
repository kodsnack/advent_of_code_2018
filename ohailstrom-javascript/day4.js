const fs = require("fs");

const getSortedGuards = input => {
  const inputArray = input.split("\n");
  const sortedArray = inputArray.sort(
    (a, b) =>
      Date.parse(a.replace("[", "").split("]")[0]) -
      Date.parse(b.replace("[", "").split("]")[0])
  );
  return sortedArray.reduce(
    (acc, val) => {
      let { currentGuard, result } = acc;
      currentGuard =
        parseInt(val.substring(val.indexOf("#") + 1)) || currentGuard;
      const minute = parseInt(val.substr(val.indexOf(":") + 1, 2));
      const isAwake = val.indexOf("wakes up") > -1;
      currentGuardObj = acc.guards[currentGuard];
      const wasAsleep =
        currentGuardObj &&
        currentGuardObj.lastAction.indexOf("falls asleep") > -1;
      const sum = (currentGuardObj && currentGuardObj.sum) || 0;
      const sleepingMinutes =
        (currentGuardObj && currentGuardObj.sleepingMinutes) ||
        new Array(60).fill().map(() => 0);
      const lastMinute = currentGuardObj && currentGuardObj.lastMinute;

      if (wasAsleep && isAwake && lastMinute) {
        for (let index = lastMinute || 0; index < minute; index++) {
          sleepingMinutes[index] = 1 + sleepingMinutes[index] || 0;
        }
      }

      acc.guards[currentGuard] = {
        id: currentGuard,
        lastAction: val,
        sleepingMinutes,
        sum: wasAsleep ? sum + (minute - currentGuardObj.lastMinute) : sum,
        lastMinute: minute
      };
      return { ...acc, currentGuard };
    },
    { guards: {} }
  );
};

const part1 = input => {
  const result = getSortedGuards(input);
  const maxGuard = Object.keys(result.guards).reduce(
    (prev, id) => (prev.sum > result.guards[id].sum ? prev : result.guards[id]),
    {}
  );
  return (
    maxGuard.sleepingMinutes.indexOf(Math.max(...maxGuard.sleepingMinutes)) *
    maxGuard.id
  );
};

const part2 = input => {
  const result = getSortedGuards(input);
  const maxSleepingMinGuard = Object.keys(result.guards).reduce(
    (prev, id) =>
      Math.max(...prev.sleepingMinutes) >
      Math.max(...result.guards[id].sleepingMinutes)
        ? prev
        : result.guards[id],
    { sleepingMinutes: [] }
  );
  return (
    maxSleepingMinGuard.sleepingMinutes.indexOf(
      Math.max(...maxSleepingMinGuard.sleepingMinutes)
    ) * maxSleepingMinGuard.id
  );
};
module.exports = { part1, part2 };
