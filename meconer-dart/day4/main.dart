import '../util/util.dart';

// const String inputFile = 'day4/example.txt';
const String inputFile = 'day4/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

int calcResultP1(String input) {
  final lines = input.split('\n');

  final eventList = lines
      .map((line) => Event(getTimeFromLine(line), line.split('] ')[1]))
      .toList();
  eventList.sort();

  int currentGuardNo = -1;
  Map<String, SleepRecord> guardSleepAtMinute = {};
  RegExp digitRe = RegExp(r'\d+');

  Time? startTime;
  for (final event in eventList) {
    if (event.event.contains('#')) {
      currentGuardNo = int.parse(digitRe.firstMatch(event.event)!.group(0)!);
    } else if (event.event.contains('falls asleep')) {
      startTime = event.time;
    } else if (event.event.contains('wakes up')) {
      final monthDay = event.time.toMonthDay();

      if (!guardSleepAtMinute.containsKey(monthDay)) {
        final sleepRecord = SleepRecord(currentGuardNo);
        guardSleepAtMinute[monthDay] = sleepRecord;
      }
      for (int minute = startTime!.minute;
          minute < event.time.minute;
          minute++) {
        guardSleepAtMinute[monthDay]!.asleepAtMinute[minute] = true;
      }
    }
  }
  Map<int, int> guardAsleepMinuteCount = {};
  guardSleepAtMinute.values.forEach((sleepRecord) {
    if (!guardAsleepMinuteCount.containsKey(sleepRecord.guardNo)) {
      guardAsleepMinuteCount[sleepRecord.guardNo] = 0;
    }
    guardAsleepMinuteCount[sleepRecord.guardNo] =
        guardAsleepMinuteCount[sleepRecord.guardNo]! +
            sleepRecord.asleepAtMinute.where((asleep) => asleep).length;
  });
  int mostSleepyGuardNo = 0;
  int sleepTimeForMostSleepyGuard = 0;
  guardAsleepMinuteCount.forEach((gNo, sleepTime) {
    if (sleepTime > sleepTimeForMostSleepyGuard) {
      mostSleepyGuardNo = gNo;
      sleepTimeForMostSleepyGuard = sleepTime;
    }
  });
  List<int> asleepAtMinuteCounts = List.generate(60, (_) => 0);
  guardSleepAtMinute.values.forEach((sleepRecord) {
    if (sleepRecord.guardNo == mostSleepyGuardNo) {
      for (int minute = 0; minute < 60; minute++) {
        if (sleepRecord.asleepAtMinute[minute]) asleepAtMinuteCounts[minute]++;
      }
    }
  });
  int sleepTimeWhenMaxSleep = 0;
  int minuteWithMaxSleep = -1;
  for (int minute = 0; minute < 60; minute++) {
    if (asleepAtMinuteCounts[minute] > sleepTimeWhenMaxSleep) {
      minuteWithMaxSleep = minute;
      sleepTimeWhenMaxSleep = asleepAtMinuteCounts[minute];
    }
  }
  return minuteWithMaxSleep * mostSleepyGuardNo;
}

int calcResultP2(String input) {
  final lines = input.split('\n');

  final eventList = lines
      .map((line) => Event(getTimeFromLine(line), line.split('] ')[1]))
      .toList();
  eventList.sort();

  int currentGuardNo = -1;
  Map<String, SleepRecord> guardSleepAtMinute = {};
  RegExp digitRe = RegExp(r'\d+');

  Time? startTime;
  for (final event in eventList) {
    if (event.event.contains('#')) {
      currentGuardNo = int.parse(digitRe.firstMatch(event.event)!.group(0)!);
    } else if (event.event.contains('falls asleep')) {
      startTime = event.time;
    } else if (event.event.contains('wakes up')) {
      final monthDay = event.time.toMonthDay();

      if (!guardSleepAtMinute.containsKey(monthDay)) {
        final sleepRecord = SleepRecord(currentGuardNo);
        guardSleepAtMinute[monthDay] = sleepRecord;
      }
      for (int minute = startTime!.minute;
          minute < event.time.minute;
          minute++) {
        guardSleepAtMinute[monthDay]!.asleepAtMinute[minute] = true;
      }
    }
  }
  // Map<int, int> guardAsleepMinuteCount = {};
  // guardSleepAtMinute.values.forEach((sleepRecord) {
  //   if (!guardAsleepMinuteCount.containsKey(sleepRecord.guardNo)) {
  //     guardAsleepMinuteCount[sleepRecord.guardNo] = 0;
  //   }
  //   guardAsleepMinuteCount[sleepRecord.guardNo] =
  //       guardAsleepMinuteCount[sleepRecord.guardNo]! +
  //           sleepRecord.asleepAtMinute.where((asleep) => asleep).length;
  // });
  Map<int, List<int>> asleepAtMinuteCounts = {};

  guardSleepAtMinute.values.forEach((sleepRecord) {
    if (!asleepAtMinuteCounts.containsKey(sleepRecord.guardNo)) {
      asleepAtMinuteCounts[sleepRecord.guardNo] = List.generate(60, (_) => 0);
    }
    for (int minute = 0; minute < 60; minute++) {
      if (sleepRecord.asleepAtMinute[minute]) {
        asleepAtMinuteCounts[sleepRecord.guardNo]![minute]++;
      }
    }
  });

  int mostSleepyMinute = -1;
  int sleepTimeAtMostSleepyMinute = 0;
  int guardNoWithMostSleepyMinute = 0;
  for (final guardNo in asleepAtMinuteCounts.keys) {
    for (int minute = 0; minute < 60; minute++) {
      if (asleepAtMinuteCounts[guardNo]![minute] >
          sleepTimeAtMostSleepyMinute) {
        sleepTimeAtMostSleepyMinute = asleepAtMinuteCounts[guardNo]![minute];
        mostSleepyMinute = minute;
        guardNoWithMostSleepyMinute = guardNo;
      }
    }
  }
  return mostSleepyMinute * guardNoWithMostSleepyMinute;
}

class SleepRecord {
  int guardNo;
  List<bool> asleepAtMinute = List.generate(60, (_) => false);
  SleepRecord(this.guardNo);
}

class Time {
  int year, month, day, hour, minute;
  Time(this.year, this.month, this.day, this.hour, this.minute);

  String toMonthDay() {
    String s = '';
    s += month < 10 ? '0' + month.toString() : month.toString();
    s += '-';
    s += day < 10 ? '0' + day.toString() : day.toString();
    return s;
  }
}

Time getTimeFromLine(String line) {
  final timePart = line.split(']')[0];
  final year = int.parse(timePart.split('-')[0].substring(1));
  final month = int.parse(timePart.split('-')[1]);
  final day = int.parse(timePart.split('-')[2].split(' ')[0]);
  final hour = int.parse(timePart.split('-')[2].split(' ')[1].split(':')[0]);
  final minute = int.parse(timePart.split('-')[2].split(' ')[1].split(':')[1]);
  return Time(year, month, day, hour, minute);
}

class Event implements Comparable<Event> {
  Time time;
  String event;
  Event(this.time, this.event);

  @override
  int compareTo(Event other) {
    int cmp = time.year.compareTo(other.time.year);
    if (cmp != 0) return cmp;
    cmp = time.month.compareTo(other.time.month);
    if (cmp != 0) return cmp;
    cmp = time.day.compareTo(other.time.day);
    if (cmp != 0) return cmp;
    cmp = time.hour.compareTo(other.time.hour);
    if (cmp != 0) return cmp;
    cmp = time.minute.compareTo(other.time.minute);
    return cmp;
  }
}
