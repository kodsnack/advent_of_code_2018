import 'dart:math';

import '../util/util.dart';

// const String inputFile = 'day5/example.txt';
const String inputFile = 'day5/input.txt';

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
  var chars = input.split('');
  bool hadReaction = true;
  while (hadReaction) {
    (chars, hadReaction) = doFirstReaction(chars);
  }
  return doReactions(chars);
}

int doReactions(List<String> chars) {
  bool hadReaction = true;
  while (hadReaction) {
    (chars, hadReaction) = doFirstReaction(chars);
  }
  return chars.length;
}

int calcResultP2(String input) {
  var chars = input.split('');
  Set<String> units = chars.map((char) => char.toLowerCase()).toSet();
  int shortestLength = veryLargeNumber;
  units.forEach((char) {
    final charsToTest = [...chars];
    charsToTest.removeWhere((c) => c.toLowerCase() == char);

    int polymerLength = doReactions(charsToTest);
    shortestLength = min(shortestLength, polymerLength);
  });

  return shortestLength;
}

(List<String>, bool) doFirstReaction(List<String> chars) {
  int idx = 0;
  while (idx < chars.length - 1) {
    String char1 = chars[idx];
    String char2 = chars[idx + 1];
    if (!sameCase(char1, char2)) {
      if (char1.toUpperCase() == char2.toUpperCase()) {
        chars.removeRange(idx, idx + 2);
        return (chars, true);
      }
    }
    idx++;
  }
  return (chars, false);
}

bool sameCase(String char1, String char2) {
  if (isUpperCase(char1) && isUpperCase(char2)) return true;
  if (!isUpperCase(char1) && !isUpperCase(char2)) return true;
  return false;
}

bool isUpperCase(String char) {
  return char == char.toUpperCase();
}
