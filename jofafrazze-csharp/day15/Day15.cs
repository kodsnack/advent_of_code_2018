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
        static List<Unit> units = new List<Unit>();

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

        static char[,] CreateUnits(char[,] map)
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

        static HashSet<Position> FindReachablePositions(Unit from, Dictionary<Position, bool> occupied)
        {
            HashSet<Position> reachable = new HashSet<Position>();
            HashSet<Position> isChecked = new HashSet<Position>();
            List<Position> toCheck = new List<Position>();
            toCheck.Add(from.pos);
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            while (toCheck.Count > 0)
            {
                HashSet<Position> newToCheck = new HashSet<Position>();
                foreach (Position p in toCheck)
                {
                    if (cleanMap[p.x, p.y] == '.')
                    {
                        bool add = !occupied.ContainsKey(p) || (from.pos == p) || (occupied[p] != from.isElf);
                        if (add)
                        {
                            reachable.Add(p);
                        }
                        bool checkAdjacent = !occupied.ContainsKey(p) || (from.pos == p);
                        if (checkAdjacent)
                        {
                            if (p.y > 0)
                            {
                                newToCheck.Add(new Position(p.x, p.y - 1));
                            }
                            if (p.x < width - 1)
                            {
                                newToCheck.Add(new Position(p.x + 1, p.y));
                            }
                            if (p.y < height - 1)
                            {
                                newToCheck.Add(new Position(p.x, p.y + 1));
                            }
                            if (p.x > 0)
                            {
                                newToCheck.Add(new Position(p.x - 1, p.y));
                            }
                        }
                    }
                    isChecked.Add(p);
                }
                toCheck.Clear();
                foreach (Position p in newToCheck)
                {
                    if (!isChecked.Contains(p))
                    {
                        toCheck.Add(p);
                    }
                }
            }
            return reachable;
        }

        static List<Unit> FindReachableUnits(Unit from, HashSet<Position> reachablePositions)
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
                    TryAddDistance(new Position(p.x, p.y - 1), p);
                    TryAddDistance(new Position(p.x + 1, p.y), p);
                    TryAddDistance(new Position(p.x, p.y + 1), p);
                    TryAddDistance(new Position(p.x - 1, p.y), p);
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
                TryGoBack(new Position(current.x, current.y - 1));
                TryGoBack(new Position(current.x - 1, current.y));
                TryGoBack(new Position(current.x + 1, current.y));
                TryGoBack(new Position(current.x, current.y + 1));
                distance = nextDistance;
                nextDistance = -1;
            }
            path.RemoveAt(0);
            return path;
        }

        static Tuple<Unit, List<Position>> FindUnitToAttack(Unit from, List<Unit> reachableUnits, HashSet<Position> reachablePositions)
        {
            List<Tuple<Unit, List<Position>>> shortestPaths = new List<Tuple<Unit, List<Position>>>();
            foreach (Unit u in reachableUnits)
            {
                List<Position> path = FindShortestPath(from.pos, u.pos, reachablePositions);
                int minDistance = (shortestPaths.Count == 0) ? int.MaxValue : shortestPaths[0].Item2.Count;
                if (path.Count < minDistance)
                {
                    shortestPaths.Clear();
                    shortestPaths.Add(Tuple.Create(u, path));
                }
                else if ((minDistance == 1) && (path.Count == 1))
                {
                    shortestPaths.Add(Tuple.Create(u, path));
                }
            }
            int index = 0;
            int minPoints = int.MaxValue;
            if (shortestPaths.Count > 1)
            {
                units = units.OrderBy(x => x.pos).ToList();
                for (int i = 0; i < shortestPaths.Count; i++)
                {
                    if (shortestPaths[i].Item1.points < minPoints)
                    {
                        minPoints = shortestPaths[i].Item1.points;
                        index = i;
                    }
                }
            }
            return shortestPaths[index];
        }

        static int ExecuteCombat(int elfAttackPower)
        {
            map = ReadInput();
            cleanMap = CreateUnits(map);
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
                Dictionary<Position, bool> occupiedPositions = new Dictionary<Position, bool>();
                foreach (Unit u in units)
                {
                    if (u.points > 0)
                    {
                        occupiedPositions[u.pos] = u.isElf;
                    }
                }
                bool lastOfAKindKilledByMostRecentUnit = false;
                foreach (Unit u in units)
                {
                    if (u.points > 0)
                    {
                        lastOfAKindKilledByMostRecentUnit = false;
                        HashSet<Position> p = FindReachablePositions(u, occupiedPositions);
                        List<Unit> reachableUnits = FindReachableUnits(u, p);
                        if (reachableUnits.Count > 0)
                        {
                            Tuple<Unit, List<Position>> toAttack = FindUnitToAttack(u, reachableUnits, p);
                            if (toAttack.Item2.Count > 1)
                            {
                                occupiedPositions.Remove(u.pos);
                                u.pos = toAttack.Item2[0];
                                occupiedPositions[u.pos] = u.isElf;
                                if (toAttack.Item2.Count == 2)
                                {
                                    toAttack = FindUnitToAttack(u, reachableUnits, p);
                                }
                            }
                            if (toAttack.Item2.Count <= 1)
                            {
                                int power = u.isElf ? elfAttackPower : 3;
                                toAttack.Item1.points -= power;
                                if (toAttack.Item1.points <= 0)
                                {
                                    toAttack.Item1.points = 0;
                                    occupiedPositions.Remove(toAttack.Item1.pos);
                                    liveElves = units.Count(a => a.isElf && (a.points > 0));
                                    liveGoblins = units.Count(a => !a.isElf && (a.points > 0));
                                    lastOfAKindKilledByMostRecentUnit =
                                        (liveElves == 0) || (liveGoblins == 0);
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
            int iter = ExecuteCombat(3);
            int sum = units.Select(x => x.points).Sum();
            int points = iter * sum;
            Console.WriteLine("Part A: Result after " + iter + " full rounds is " + points + ".");
        }

        static void PartB()
        {
            map = ReadInput();
            int elvesBefore = units.Where(x => x.isElf).Count();
            int elvesAfter = 0;
            int elfPower = 3;
            int iter = 0;
            int sum = 0;
            int points = 0;
            while (elvesAfter != elvesBefore)
            {
                iter = ExecuteCombat(elfPower++);
                elvesAfter = units.Where(x => x.isElf && (x.points > 0)).Count();
                sum = units.Select(x => x.points).Sum();
                points = iter * sum;
                //Console.Write(".");
                Console.WriteLine("Part B: Result after " + iter + " full rounds is " + points +
                    " (using Elf attack power of " + (elfPower - 1) + ").");
            }
            //Console.WriteLine();
            Console.WriteLine("Part B: Result after " + iter + " full rounds is " + points + 
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
