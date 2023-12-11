import '../util/linepos.dart';
import '../util/util.dart';

// const String inputFile = 'day3/example.txt';
const String inputFile = 'day3/input.txt';

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
  Map<LinePos, int> claimCount = {};
  for (final line in lines) {
    int colStart, rowStart, width, height;
    (_, colStart, rowStart, width, height) = getRectangle(line);
    for (int ofsCol = 0; ofsCol < width; ofsCol++) {
      for (int ofsRow = 0; ofsRow < height; ofsRow++) {
        final pos = LinePos(ofsCol + colStart, ofsRow + rowStart);
        int count = claimCount[pos] ?? 0;
        count++;
        claimCount[pos] = count;
      }
    }
  }

  final result = claimCount.values.where((element) => element > 1).length;
  return result;
}

int calcResultP2(String input) {
  final lines = input.split('\n');
  Map<LinePos, int> claimCount = {};
  for (final line in lines) {
    int colStart, rowStart, width, height;
    (_, colStart, rowStart, width, height) = getRectangle(line);
    for (int ofsCol = 0; ofsCol < width; ofsCol++) {
      for (int ofsRow = 0; ofsRow < height; ofsRow++) {
        final pos = LinePos(ofsCol + colStart, ofsRow + rowStart);
        int count = claimCount[pos] ?? 0;
        count++;
        claimCount[pos] = count;
      }
    }
  }
  int notOverlappingId = -1;
  for (final line in lines) {
    int id, colStart, rowStart, width, height;
    (id, colStart, rowStart, width, height) = getRectangle(line);
    bool overlapping = false;
    for (int ofsCol = 0; ofsCol < width; ofsCol++) {
      for (int ofsRow = 0; ofsRow < height; ofsRow++) {
        final pos = LinePos(ofsCol + colStart, ofsRow + rowStart);
        int count = claimCount[pos] ?? 0;
        if (count > 1) overlapping = true;
      }
    }
    if (!overlapping) notOverlappingId = id;
  }

  return notOverlappingId;
}

(int, int, int, int, int) getRectangle(String line) {
  int id = int.parse(line.split(' ')[0].substring(1));
  line = line.substring(line.indexOf('@ ') + 1);
  int colStart = int.parse(line.split(',')[0]);
  int rowStart = int.parse(line.split(',')[1].split(':')[0]);
  int width = int.parse(line.split(': ')[1].split('x')[0]);
  int height = int.parse(line.split(': ')[1].split('x')[1]);
  return (id, colStart, rowStart, width, height);
}
