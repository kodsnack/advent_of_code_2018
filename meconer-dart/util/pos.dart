class Pos {
  final int x, y;
  Pos(this.x, this.y);

  Pos moveUp({int dist = 1}) {
    return Pos(x, y + dist);
  }

  Pos moveDown({int dist = 1}) {
    return Pos(x, y - dist);
  }

  Pos moveLeft({int dist = 1}) {
    return Pos(x - dist, y);
  }

  Pos moveRight({int dist = 1}) {
    return Pos(x + dist, y);
  }

  Pos moveDir(String command, {int dist = 1}) {
    switch (command) {
      case 'U':
        return moveUp(dist: dist);
      case 'D':
        return moveDown(dist: dist);
      case 'L':
        return moveLeft(dist: dist);
      case 'R':
        return moveRight(dist: dist);
      default:
        throw Exception('Wrong command');
    }
  }

  @override
  bool operator ==(Object other) {
    if (identical(this, other)) return true;
    return other is Pos && (x == other.x && y == other.y);
  }

  int get hashCode => x * 100000 + y;

  moveDirWithLimit(String dir, int limitx, int limity, {int dist = 1}) {
    Pos pos = moveDir(dir, dist: dist);
    if (pos.x < 0) return Pos(0, pos.y);
    if (pos.x > limitx) return Pos(limitx, pos.y);
    if (pos.y < 0) return Pos(pos.x, 0);
    if (pos.y > limity) return Pos(pos.x, limity);
    return pos;
  }

  int manhattanDistance(Pos pos) {
    int xDist = (pos.x - x).abs();
    int yDist = (pos.y - y).abs();
    return xDist + yDist;
  }

  List<Pos> getNeighbours() {
    return [moveUp(), moveDown(), moveLeft(), moveRight()];
  }
}
