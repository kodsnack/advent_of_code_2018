using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day15
{
    public struct Position : IComparable<Position>
    {
        public int x;
        public int y;

        public Position(Position p)
        {
            x = p.x;
            y = p.y;
        }
        public Position(int x, int y)
        {
            this.x = x;
            this.y = y;
        }
        public int CompareTo(Position p)    // Reading order
        {
            if (x == p.x && y == p.y)
                return 0;
            else if (y == p.y)
                return (x < p.x) ? -1 : 1;
            else
                return (y < p.y) ? -1 : 1;
        }
        public override bool Equals(Object obj)
        {
            return obj is Position && Equals((Position)obj);
        }
        public bool Equals(Position p)
        {
            return (x == p.x) && (y == p.y);
        }
        public override int GetHashCode()
        {
            var hashCode = 1502939027;
            hashCode = hashCode * -1521134295 + base.GetHashCode();
            hashCode = hashCode * -1521134295 + x.GetHashCode();
            hashCode = hashCode * -1521134295 + y.GetHashCode();
            return hashCode;
        }
        public static bool operator ==(Position p1, Position p2)
        {
            return p1.Equals(p2);
        }
        public static bool operator !=(Position p1, Position p2)
        {
            return !p1.Equals(p2);
        }
        public static bool operator <(Position p1, Position p2)
        {
            return p1.CompareTo(p2) == -1;
        }
        public static bool operator >(Position p1, Position p2)
        {
            return p1.CompareTo(p2) == 1;
        }
        public static Position operator +(Position p1, Position p2)
        {
            Position p = new Position(p1);
            p.x += p2.x;
            p.y += p2.y;
            return p;
        }
    }

    public class Unit
    {
        public bool isElf;
        public Position pos;
        public int points;

        public Unit(bool isE, Position p)
        {
            isElf = isE;
            pos = p;
            points = 200;
        }
    }

    class Day15
    {
        static char[,] map;
        static char[,] cleanMap;

        static void PrintMap(char[,] map, List<Unit> units)
        {
            char[,] printMap = (char[,])map.Clone();
            foreach (Unit u in units)
            {
                if (u.points > 0)
                {
                    printMap[u.pos.x, u.pos.y] = u.isElf ? 'E' : 'G';
                }
            }
            int width = printMap.GetLength(0);
            int height = printMap.GetLength(1);
            for (int y = 0; y < height; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < width; x++)
                {
                    sb.Append(printMap[x, y]);
                }
                Console.Write(sb.ToString());
                List<Unit> rowUnits = units.Where(u => (u.pos.y == y) && (u.points > 0)).ToList();
                if (rowUnits.Count > 0)
                {
                    Console.Write("  ");
                    foreach (Unit u in rowUnits)
                    {
                        Console.Write((u.isElf ? "E" : "G") + "(" + u.points + ") ");
                    }
                }
                Console.WriteLine();
            }
            Console.WriteLine();
        }

        static void PrintMap(char[,] map)
        {
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            for (int y = 0; y < height; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < width; x++)
                {
                    sb.Append(map[x, y]);
                }
                Console.WriteLine(sb.ToString());
            }
            //Console.WriteLine();
        }

        static char[,] ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            int w = -1;
            int h = 0;
            while ((line = reader.ReadLine()) != null)
            {
                if (w < 0)
                {
                    w = line.Length;
                }
                h++;
            }
            reader.DiscardBufferedData();
            reader.BaseStream.Seek(0, System.IO.SeekOrigin.Begin);
            char[,] data = new char[w, h];
            int y = 0;
            while ((line = reader.ReadLine()) != null)
            {
                int x = 0;
                foreach (char c in line)
                {
                    data[x++, y] = c;
                }
                y++;
            }
            return data;
        }

        static char[,] CreateUnits(char[,] map, ref List<Unit> units)
        {
            char[,] m = (char[,])map.Clone();
            units.Clear();
            int width = m.GetLength(0);
            int height = m.GetLength(1);
            for (int x = 0; x < width; x++)
            {
                for (int y = 0; y < height; y++)
                {
                    char c = m[x, y];
                    if ((c == 'E') || (c == 'G'))
                    {
                        Position p = new Position(x, y);
                        Unit u = new Unit((c == 'E'), p);
                        units.Add(u);
                        m[x, y] = '.';
                    }
                }
            }
            return m;
        }

        static HashSet<Position> FindReachablePositions(Unit from, Dictionary<Position, Unit> occupied)
        {
            HashSet<Position> reachable = new HashSet<Position>();
            HashSet<Position> isChecked = new HashSet<Position>();
            List<Position> toCheck = new List<Position>();
            toCheck.Add(from.pos);
            int xMax = map.GetLength(0) - 1;
            int yMax = map.GetLength(1) - 1;
            while (toCheck.Count > 0)
            {
                HashSet<Position> newToCheck = new HashSet<Position>();
                void MaybeAddPosition(Position p)
                {
                    if (!isChecked.Contains(p) && (p.x > 0) && (p.x < xMax) && (p.y > 0) && (p.y < yMax))
                    {
                        newToCheck.Add(p);
                    }
                }
                foreach (Position p in toCheck)
                {
                    if (cleanMap[p.x, p.y] == '.')
                    {
                        bool add = !occupied.ContainsKey(p) || (from.pos == p) || (occupied[p].isElf != from.isElf);
                        if (add)
                        {
                            reachable.Add(p);
                        }
                        bool checkAdjacent = !occupied.ContainsKey(p) || (from.pos == p);
                        if (checkAdjacent)
                        {
                            MaybeAddPosition(p + goUp);
                            MaybeAddPosition(p + goRight);
                            MaybeAddPosition(p + goDown);
                            MaybeAddPosition(p + goLeft);
                        }
                    }
                    isChecked.Add(p);
                }
                toCheck = new List<Position>(newToCheck);
            }
            return reachable;
        }

        static List<Unit> FindReachableUnits(Unit from, List<Unit> units, HashSet<Position> reachablePositions)
        {
            List<Unit> reachable = new List<Unit>();
            foreach (Unit u in units)
            {
                if ((from != u) && (u.points > 0) &&
                    reachablePositions.Contains(u.pos) && (u.isElf != from.isElf))
                {
                    reachable.Add(u);
                }
            }
            return reachable;
        }

        static readonly Position goUp = new Position(0, -1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDown = new Position(0, 1);
        static readonly List<Position> allDirections = new List<Position>() { goUp, goLeft, goRight, goDown };

        static List<Position> FindShortestPath(Position p1, Position p2, HashSet<Position> reachable)
        {
            Dictionary<Position, int> distances = new Dictionary<Position, int>();
            List<Position> toCheck = new List<Position>();
            List<Position> newToCheck = new List<Position>();
            toCheck.Add(p1);
            distances[p1] = 0;
            void TryAddDistance(Position to, Position from)
            {
                if (reachable.Contains(to) && !distances.ContainsKey(to))
                {
                    distances[to] = distances[from] + 1;
                    newToCheck.Add(to);
                }
            }
            while (!distances.ContainsKey(p2))
            {
                foreach (Position p in toCheck)
                {
                    TryAddDistance(p + goUp, p);
                    TryAddDistance(p + goRight, p);
                    TryAddDistance(p + goDown, p);
                    TryAddDistance(p + goLeft, p);
                }
                toCheck = new List<Position>(newToCheck);
                newToCheck.Clear();
            }
            List<Position> path = new List<Position>();
            int distance = distances[p2];
            int nextDistance = -1;
            Position current = p2;
            path.Add(p2);
            void TryGoBack(Position p)
            {
                if (nextDistance < 0)
                {
                    if (distances.ContainsKey(p) && (distances[p] == distance - 1))
                    {
                        path.Insert(0, p);
                        nextDistance = distance - 1;
                        current = p;
                    }
                }
            }
            while (current != p1)
            {
                TryGoBack(current + goUp);
                TryGoBack(current + goLeft);
                TryGoBack(current + goRight);
                TryGoBack(current + goDown);
                distance = nextDistance;
                nextDistance = -1;
            }
            path.RemoveAt(0);
            return path;
        }

        static List<Position> FindSingleShortestPath(Unit from, SortedSet<Position> attackPositions, HashSet<Position> reachablePositions)
        {
            List<Position> shortestPath = new List<Position>();
            int minDistance = int.MaxValue;
            foreach (Position attackPos in attackPositions)
            {
                List<Position> path = FindShortestPath(from.pos, attackPos, reachablePositions);
                if (path.Count < minDistance)
                {
                    minDistance = path.Count;
                    shortestPath = path;
                }
            }
            return shortestPath;
        }

        static SortedSet<Position> FindReachableSquares(List<Unit> units, HashSet<Position> positions)
        {
            SortedSet<Position> squares = new SortedSet<Position>();
            foreach (Unit u in units)
            {
                foreach (Position d in allDirections)
                {
                    if (positions.Contains(u.pos + d))
                    {
                        squares.Add(u.pos + d);
                    }
                }
            }
            return squares;
        }

        static Tuple<bool, Position> FindMovementBeforeAttack(Unit from, List <Unit> reachableUnits, HashSet<Position> reachablePositions)
        {
            SortedSet<Position> reachableSquares = FindReachableSquares(reachableUnits, reachablePositions);
            List<Position> path = FindSingleShortestPath(from, reachableSquares, reachablePositions);
            return (path.Count >= 1) ? Tuple.Create(true, path[0]) : Tuple.Create(false, new Position());
        }

        static Tuple<bool, Unit> FindUnitToAttack(Unit from, Dictionary<Position, Unit> occupied)
        {
            List<Unit> possibleAttacks = new List<Unit>();
            foreach (Position d in allDirections)
            {
                Position p = from.pos + d;
                if (occupied.ContainsKey(p) && (occupied[p].isElf != from.isElf))
                {
                    possibleAttacks.Add(occupied[p]);
                }
            }
            if (possibleAttacks.Count > 0)
            {
                Unit attack = possibleAttacks[0];
                foreach (Unit u in possibleAttacks)
                {
                    if (u.points < attack.points)
                    {
                        attack = u;
                    }
                }
                return Tuple.Create(true, attack);
            }
            return Tuple.Create(false, from);
        }

        static int ExecuteCombat(List<Unit> units, int elfAttackPower)
        {
            units = units.OrderBy(x => x.pos).ToList();
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            int iter = 0;
            int liveElves = units.Count(a => a.isElf && (a.points > 0));
            int liveGoblins = units.Count(a => !a.isElf && (a.points > 0));
            bool bothKindsAlive = true;
            //Console.WriteLine("Initial state:");
            //PrintMap(cleanMap, units);
            while (bothKindsAlive)
            {
                Dictionary<Position, Unit> occupiedPositions = new Dictionary<Position, Unit>();
                foreach (Unit u in units)
                {
                    if (u.points > 0)
                    {
                        occupiedPositions[u.pos] = u;
                    }
                }
                bool lastOfAKindKilledByMostRecentUnit = false;
                List<Unit> alwaysSortedUnits = units.OrderBy(x => x.pos).ToList();
                foreach (Unit u in units)
                {
                    if (u.points > 0)
                    {
                        lastOfAKindKilledByMostRecentUnit = false;
                        HashSet<Position> p = FindReachablePositions(u, occupiedPositions);
                        List<Unit> reachableUnits = FindReachableUnits(u, alwaysSortedUnits, p);
                        if (reachableUnits.Count > 0)
                        {
                            Tuple<bool, Position> move = FindMovementBeforeAttack(u, reachableUnits, p);
                            if (move.Item1)
                            {
                                occupiedPositions.Remove(u.pos);
                                u.pos = move.Item2;
                                occupiedPositions[u.pos] = u;
                                alwaysSortedUnits = alwaysSortedUnits.OrderBy(x => x.pos).ToList();
                            }
                            Tuple<bool, Unit> attack = FindUnitToAttack(u, occupiedPositions);
                            if (attack.Item1)
                            {
                                int power = u.isElf ? elfAttackPower : 3;
                                attack.Item2.points -= power;
                                if (attack.Item2.points <= 0)
                                {
                                    attack.Item2.points = 0;
                                    occupiedPositions.Remove(attack.Item2.pos);
                                    liveElves = units.Count(a => a.isElf && (a.points > 0));
                                    liveGoblins = units.Count(a => !a.isElf && (a.points > 0));
                                    lastOfAKindKilledByMostRecentUnit = (liveElves == 0) || (liveGoblins == 0);
                                }
                            }
                        }
                    }
                }
                bothKindsAlive = (liveElves > 0) && (liveGoblins > 0);
                if (bothKindsAlive || lastOfAKindKilledByMostRecentUnit)
                {
                    iter++;
                }
                units = units.OrderBy(x => x.pos).ToList();
                //Console.WriteLine("After " + iter + " rounds:");
                //PrintMap(cleanMap, units);
            }
            //Console.WriteLine("After " + iter + " rounds:");
            //PrintMap(cleanMap, units);
            return iter;
        }

        static void PartA()
        {
            map = ReadInput();
            List<Unit> units = new List<Unit>();
            cleanMap = CreateUnits(map, ref units);
            int iter = ExecuteCombat(units, 3);
            int sum = units.Select(x => x.points).Sum();
            int points = iter * sum;
            Console.WriteLine("Part A: Result after " + iter + " full rounds is " + points + ".");
        }

        static void PartB()
        {
            map = ReadInput();
            List<Unit> units = new List<Unit>();
            cleanMap = CreateUnits(map, ref units);
            int elvesBefore = units.Where(x => x.isElf).Count();
            int elvesAfter = 0;
            int elfPower = 3;
            int iter = 0;
            int sum = 0;
            int points = 0;
            while (elvesAfter != elvesBefore)
            {
                cleanMap = CreateUnits(map, ref units);
                iter = ExecuteCombat(units, elfPower++);
                elvesAfter = units.Where(x => x.isElf && (x.points > 0)).Count();
                sum = units.Select(x => x.points).Sum();
                points = iter * sum;
                //Console.Write(".");
                Console.WriteLine("Part B: Result after " + iter + " full rounds is " + points +
                    " (using Elf attack power of " + (elfPower - 1) + ").");
            }
            //Console.WriteLine();
            Console.WriteLine("Part B: Final result after " + iter + " full rounds is " + points +
                " (using Elf attack power of " + (elfPower - 1) + ").");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day15).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
