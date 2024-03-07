import '../util/util.dart';

// const String inputFile = 'day7/example.txt';
const String inputFile = 'day7/input.txt';

class Problem {
  bool isExample = false;
  int dayNo = 7;

  Future<String> getInput() async {
    String dirName = 'day$dayNo/';
    String fileName = dirName + (isExample ? 'example.txt' : 'input.txt');
    return readInputAsString(fileName);
  }

  int get numberOfWorkers {
    return isExample ? 2 : 5;
  }

  int getTimeToFinishItem(String item) {
    if (isExample) {
      return item.codeUnitAt(0) - "A".codeUnitAt(0) + 1;
    } else {
      return item.codeUnitAt(0) - "A".codeUnitAt(0) + 61;
    }
  }
}

Problem problem = Problem();

Future<void> main(List<String> args) async {
  var input = await problem.getInput();

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

String calcResultP1(String input) {
  Map<String, List<String>> nodeMap = {};
  Set<String> unFinishedSteps = nodeMap.keys.toSet();
  for (final line in input.split('\n')) {
    final parts = line.split(' ');
    String step = parts[7];
    unFinishedSteps.add(step);

    String preStep = parts[1];
    unFinishedSteps.add(preStep);

    if (nodeMap.containsKey(step)) {
      nodeMap[step]!.add(preStep);
    } else {
      nodeMap[step] = [preStep];
    }
  }

  List<String> finishedSteps = [];
  while (unFinishedSteps.isNotEmpty) {
    // Find available steps
    List<String> availableSteps = [];

    for (final step in unFinishedSteps) {
      bool isAvailable = true;
      if (nodeMap.containsKey(step)) {
        for (final pre in nodeMap[step]!) {
          if (unFinishedSteps.contains(pre)) {
            isAvailable = false;
            break;
          }
        }
      }
      if (isAvailable) availableSteps.add(step);
    }
    availableSteps.sort();
    unFinishedSteps.remove(availableSteps.first);
    finishedSteps.add(availableSteps.first);
  }

  return finishedSteps.join('');
}

class Step {
  String name;
  int finishTime;

  Step(this.name, this.finishTime);
}

int calcResultP2(String input) {
  Map<String, List<String>> conditions;
  Set<String> unFinishedSteps;

  (conditions, unFinishedSteps) = parseInput(input);

  List<String> finishedSteps = [];
  List<Step> stepsAtWork = [];

  int noOfFreeWorkers = problem.numberOfWorkers;

  int currentTime = 0;

  while (unFinishedSteps.isNotEmpty || stepsAtWork.isNotEmpty) {
    // Find available steps
    List<String> availableSteps =
        getAvailableSteps(unFinishedSteps, conditions, stepsAtWork);

    // Sort in alphabetic order.
    availableSteps.sort();

    // Put free workers to work with available steps
    while (noOfFreeWorkers > 0 && availableSteps.isNotEmpty) {
      final stepName = availableSteps.removeAt(0);
      noOfFreeWorkers--;
      final timeWhenFinished = currentTime + getTimeToFinishStep(stepName);
      stepsAtWork.add(Step(stepName, timeWhenFinished));
      stepsAtWork.sort((a, b) => a.finishTime.compareTo(b.finishTime));
    }

    // No free workers or no available steps so we move forward in time to
    // when the next step is finished
    final step = stepsAtWork.removeAt(0);
    noOfFreeWorkers++;
    currentTime = step.finishTime;
    finishedSteps.add(step.name);
    unFinishedSteps.remove(step.name);
  }

  return currentTime;
}

List<String> getAvailableSteps(Set<String> unFinishedSteps,
    Map<String, List<String>> nodeMap, List<Step> stepsAtWork) {
  List<String> availableSteps = [];
  for (final step in unFinishedSteps) {
    bool isAvailable = true;
    if (nodeMap.containsKey(step)) {
      for (final pre in nodeMap[step]!) {
        if (unFinishedSteps.contains(pre)) {
          isAvailable = false;
          break;
        }
      }
    }
    // Remove steps already at work
    availableSteps = availableSteps
        .where((stepName) => !stepsAtWork.any((step) => step.name == stepName))
        .toList();
    isAvailable = isAvailable && !(stepsAtWork.contains(step));

    if (isAvailable) availableSteps.add(step);
  }
  return availableSteps;
}

int getTimeToFinishStep(String step) {
  if (problem.isExample) return step.codeUnitAt(0) - 'A'.codeUnitAt(0) + 1;
  return 61 + step.codeUnitAt(0) - 'A'.codeUnitAt(0);
}

(Map<String, List<String>>, Set<String>) parseInput(String input) {
  Map<String, List<String>> nodeMap = {};
  Set<String> unFinishedSteps = nodeMap.keys.toSet();
  for (final line in input.split('\n')) {
    final parts = line.split(' ');
    String step = parts[7];
    unFinishedSteps.add(step);

    String preStep = parts[1];
    unFinishedSteps.add(preStep);

    if (nodeMap.containsKey(step)) {
      nodeMap[step]!.add(preStep);
    } else {
      nodeMap[step] = [preStep];
    }
  }
  return (nodeMap, unFinishedSteps);
}
