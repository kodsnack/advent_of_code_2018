import 'dart:math';

import '../util/linepos.dart';
import '../util/lprange.dart';
import '../util/util.dart';

// const String inputFile = 'day6/example.txt';
const String inputFile = 'day6/input.txt';

Future<void> main(List<String> args) async {
  var input = await readInputAsString(inputFile);

  print('Part 1:');
  final resultP1 = calcResultP1(input);
  print(resultP1);

  print('Part 2:');
  final resultP2 = calcResultP2(input);
  print(resultP2);
}

// 3647
int calcResultP1(String input) {
  List<LinePos> points = [];
  LPRange range = LPRange();
  for (final line in input.split('\n')) {
    points.add(getPointFromLine(line));
    range.extend(points.last);
  }
  // printPoints(points, range);
  Map<LinePos, int> belongings = calculateBelongings(points, range);
  Set<int> enclosedPoints = findEnclosedPoints(points, belongings, range);
  int maxEnclosedArea = 0;
  for (final pointNo in enclosedPoints) {
    int noOfOwnedPoints =
        findNoOfOwnedPointsFor(pointNo, points, belongings, range);
    maxEnclosedArea = max(maxEnclosedArea, noOfOwnedPoints);
    // print('$pointNo : $noOfOwnedPoints');
  }

  return maxEnclosedArea;
}

// 41605
int calcResultP2(String input) {
  List<LinePos> points = [];
  LPRange range = LPRange();
  for (final line in input.split('\n')) {
    points.add(getPointFromLine(line));
    range.extend(points.last);
  }
  // printPoints(points, range);
  return findSafestAreaSize(points, range, 10000);
}

int findSafestAreaSize(List<LinePos> points, LPRange range, int maxDist) {
  int areaSize = 0;
  for (int col = range.colMin; col < range.colMax; col++) {
    for (int row = range.rowMin; row < range.rowMax; row++) {
      int totalMDist = 0;
      for (final point in points) {
        totalMDist += point.manhattanDistance(LinePos(col, row));
        if (totalMDist >= maxDist) {
          break;
        }
      }
      if (totalMDist < maxDist) areaSize++;
    }
  }
  return areaSize;
}

int findNoOfOwnedPointsFor(int pointNo, List<LinePos> points,
    Map<LinePos, int> belongings, LPRange range) {
  int count = 0;
  for (int col = range.colMin; col < range.colMax + 1; col++) {
    for (int row = range.colMin; row < range.colMax + 1; row++) {
      if (belongings[LinePos(col, row)] == pointNo) count++;
    }
  }
  return count;
}

Set<int> findEnclosedPoints(
    List<LinePos> points, Map<LinePos, int> belongings, LPRange range) {
  // Check points on the edge. Any point that owns a point on the edge has infinite area
  Set<int> edgeBelongings = {};
  for (int col = range.colMin - 1; col <= range.colMax + 1; col++) {
    edgeBelongings.add(belongings[LinePos(col, range.rowMin - 1)]!);
    edgeBelongings.add(belongings[LinePos(col, range.rowMax + 1)]!);
  }
  for (int row = range.rowMin - 1; row <= range.rowMax + 1; row++) {
    edgeBelongings.add(belongings[LinePos(range.colMin - 1, row)]!);
    edgeBelongings.add(belongings[LinePos(range.colMax + 1, row)]!);
  }

  Set<int> enclosedPoints =
      List.generate(points.length, (index) => index).toSet();
  enclosedPoints.removeAll(edgeBelongings);
  return enclosedPoints;
}

Map<LinePos, int> calculateBelongings(List<LinePos> points, LPRange range) {
  Map<LinePos, int> belongings = {};
  for (int row = range.rowMin - 1; row <= range.rowMax + 1; row++) {
    for (var col = range.colMin - 1; col <= range.colMax + 1; col++) {
      int pointNo = findPointClosestTo(LinePos(col, row), points);
      belongings[LinePos(col, row)] = pointNo;
    }
  }
  return belongings;
}

int findPointClosestTo(LinePos point, List<LinePos> points) {
  int minDist = veryLargeNumber;
  int pointNo = -1;
  for (int pointIdx = 0; pointIdx < points.length; pointIdx++) {
    int dist = point.manhattanDistance(points[pointIdx]);
    if (dist == minDist) {
      // We already have a point with this distance. Mark it as not belonged
      pointNo = -1;
    }
    if (dist < minDist) {
      minDist = dist;
      pointNo = pointIdx;
    }
  }
  return pointNo;
}

void printPoints(List<LinePos> points, LPRange range) {
  for (int row = range.rowMin - 1; row <= range.rowMax + 1; row++) {
    String line = '';
    for (var col = range.colMin - 1; col <= range.colMax + 1; col++) {
      line += points.contains(LinePos(col, row)) ? '#' : '.';
    }
    print(line);
  }
}

LinePos getPointFromLine(String line) {
  final numbers = line.split(', ').map((e) => int.parse(e));
  return LinePos(numbers.first, numbers.last);
}
