import 'dart:math';

import 'linepos.dart';
import 'util.dart';

class LPRange {
  int colMin = veryLargeNumber;
  int colMax = -veryLargeNumber;
  int rowMin = veryLargeNumber;
  int rowMax = -veryLargeNumber;

  void extend(LinePos point) {
    colMin = min(colMin, point.col);
    colMax = max(colMax, point.col);
    rowMin = min(rowMin, point.row);
    rowMax = max(rowMax, point.row);
  }

  bool contains(LinePos pos) {
    if (pos.col < colMin || pos.col > colMax) return false;
    if (pos.row < rowMin || pos.row > rowMax) return false;
    return true;
  }
}
