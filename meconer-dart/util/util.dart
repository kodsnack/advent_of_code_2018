import 'dart:io';

Future<List<String>> readInput(String fileName) {
  final file = File(fileName);
  return file.readAsLines();
}

Future<String> readInputAsString(String fileName) {
  final file = File(fileName);
  return file.readAsString();
}

const int veryLargeNumber = 99999999999999;

bool isDigit(String s, int idx) => (s.codeUnitAt(idx) ^ 0x30) <= 9;

bool isAlpha(String? s) {
  if (s == null) return false;
  RegExp _alpha = new RegExp(r'^[a-zA-Z]+$');
  return _alpha.hasMatch(s);
}

// From Wikipedia, Heaps algorithm
List<List<int>> getPermutations(List<int> startList) {
  List<List<int>> perms = [];

  void _swap(List<int> arr, int idx1, int idx2) {
    int temp = arr[idx1];
    arr[idx1] = arr[idx2];
    arr[idx2] = temp;
  }

  void permGenerate(int k, List<int> arr) {
    if (k == 1) {
      perms.add([...arr]);
      return;
    }

    // Generate permutations with k-th unaltered
    // Initially k = length(A)
    permGenerate(k - 1, arr);

    // Generate permutations for k-th swapped with each k-1 initial
    for (int i = 0; i < k - 1; i++) {
      // Swap choice dependent on parity of k (even or odd)
      if (k % 2 == 0) {
        // Even
        _swap(arr, i, k - 1);
      } else {
        _swap(arr, 0, k - 1);
      }
      permGenerate(k - 1, arr);
    }
  }

  permGenerate(startList.length, startList);
  return perms;
}

int findGcd(int n1, int n2) {
  if (n1 == n2) return n1;
  int larger = n1.abs();
  int smaller = n2.abs();
  if (larger < smaller) {
    larger = n2.abs();
    smaller = n1.abs();
  }
  while (smaller != 0) {
    int temp = smaller;
    smaller = larger % smaller;
    larger = temp;
  }
  return larger;
}
