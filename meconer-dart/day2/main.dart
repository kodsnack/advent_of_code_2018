import '../util/util.dart';

// const String inputFile = 'day2/example.txt';
const String inputFile = 'day2/input.txt';

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
  int doubleCount = 0;
  int tripleCount = 0;
  for (final line in lines) {
    final chars = line.split('');
    Map<String, int> charCounts = {};
    for (final char in chars) {
      if (charCounts.keys.contains(char)) {
        charCounts[char] = charCounts[char]! + 1;
      } else {
        charCounts[char] = 1;
      }
    }
    if (charCounts.values.contains(2)) doubleCount++;
    if (charCounts.values.contains(3)) tripleCount++;
  }
  return doubleCount * tripleCount;
}

String calcResultP2(String input) {
  final lines = input.split('\n');
  lines.sort();

  for (int idx1 = 0; idx1 < lines.length - 1; idx1++) {
    final lines1 = lines[idx1];
    final chars1 = lines1.split('');
    for (int idx2 = idx1 + 1; idx2 < lines.length; idx2++) {
      final lines2 = lines[idx2];

      final chars2 = lines2.split('');
      int charIdx = 0;
      int diffCount = 0;
      int diffIdx = 0;
      while (diffCount <= 1 && charIdx < chars1.length) {
        if (chars1[charIdx] != chars2[charIdx]) {
          diffCount++;
          diffIdx = charIdx;
        }
        charIdx++;
      }
      if (diffCount == 1) {
        chars1.removeAt(diffIdx);
        return chars1.join('');
      }
    }
  }
  return '';
}

List<int> getNumbers(List<String> lines) {
  final numbers = lines.map((e) {
    final factor = e.substring(0, 1) == '-' ? -1 : 1;
    final number = int.parse(e.substring(1));
    return factor * number;
  }).toList();
  return numbers;
}
