import '../util/util.dart';

// const String inputFile = 'day1/example.txt';
const String inputFile = 'day1/input.txt';

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
  final numbers = getNumbers(lines);

  int result =
      numbers.fold(0, (previousValue, element) => previousValue + element);
  return result;
}

int calcResultP2(String input) {
  final lines = input.split('\n');
  final numbers = getNumbers(lines);

  Set<int> seenFrequencies = {};
  int idx = 0;
  int freq = 0;
  while (true) {
    freq += numbers[idx];
    if (seenFrequencies.contains(freq)) break;
    seenFrequencies.add(freq);
    idx++;
    idx %= numbers.length;
  }
  int result = freq;
  return result;
}

List<int> getNumbers(List<String> lines) {
  final numbers = lines.map((e) {
    final factor = e.substring(0, 1) == '-' ? -1 : 1;
    final number = int.parse(e.substring(1));
    return factor * number;
  }).toList();
  return numbers;
}
