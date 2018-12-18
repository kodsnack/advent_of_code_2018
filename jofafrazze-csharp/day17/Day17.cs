using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day17
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
        public static Position operator +(Position p1, Position p2)
        {
            Position p = new Position(p1);
            p.x += p2.x;
            p.y += p2.y;
            return p;
        }
    }

    class Day17
    {
        static char[,] map;
        static Position mapWell = new Position();
        static HashSet<Position> clay = new HashSet<Position>();

        static char[,] ReadInput()
        {
            clay.Clear();
            List<Tuple<Position, Position>> lines = new List<Tuple<Position, Position>>();
            Regex regex = new Regex(@"^([xy])=(\d+), [xy]=(\d+)\.+(\d+)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = regex.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    int i = 1;
                    bool xFirst = (groups[i++].Value == "x");
                    int a = int.Parse(groups[i++].Value);
                    int b = int.Parse(groups[i++].Value);
                    int c = int.Parse(groups[i++].Value);
                    if (xFirst)
                    {
                        lines.Add(Tuple.Create(new Position(a, b), new Position(a, c)));
                    }
                    else
                    {
                        lines.Add(Tuple.Create(new Position(b, a), new Position(c, a)));
                    }
                }
            }
            Position pWell = new Position(500, 0);
            int xMin = Math.Min(pWell.x, Math.Min(lines.Select(a => a.Item1.x).Min(), lines.Select(a => a.Item2.x).Min()));
            int xMax = Math.Max(pWell.x, Math.Max(lines.Select(a => a.Item1.x).Max(), lines.Select(a => a.Item2.x).Max()));
            int yMin = Math.Min(pWell.y, Math.Min(lines.Select(a => a.Item1.y).Min(), lines.Select(a => a.Item2.y).Min()));
            int yMax = Math.Max(pWell.y, Math.Max(lines.Select(a => a.Item1.y).Max(), lines.Select(a => a.Item2.y).Max()));
            int w = xMax - xMin + 1 + 2;
            int h = yMax - yMin + 1;
            int xOrigo = xMin - 1;
            int yOrigo = yMin;
            char[,] data = new char[w, h];
            for (int i = 0; i < w * h; i++)
            {
                data[i % w, i / w] = '.';
            }
            data[pWell.x - xOrigo, pWell.y - yOrigo] = '+';
            mapWell.x = pWell.x - xOrigo;
            mapWell.y = pWell.y - yOrigo;
            Position p = new Position();
            foreach (Tuple<Position, Position> t in lines)
            {
                if (t.Item1.x == t.Item2.x)
                {
                    p.x = t.Item1.x - xOrigo;
                    for (int y = t.Item1.y; y <= t.Item2.y; y++)
                    {
                        p.y = y - yOrigo;
                        data[p.x, p.y] = '#';
                        clay.Add(p);
                    }
                }
                else
                {
                    p.y = t.Item1.y - yOrigo;
                    for (int x = t.Item1.x; x <= t.Item2.x; x++)
                    {
                        p.x = x - xOrigo;
                        data[p.x, p.y] = '#';
                        clay.Add(p);
                    }
                }
            }
            return data;
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
        }

        static void SaveMap(char[,] map, string fileName)
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\" + fileName);
            StreamWriter writer = new StreamWriter(path);
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            for (int y = 0; y < height; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < width; x++)
                {
                    sb.Append(map[x, y]);
                }
                writer.WriteLine(sb.ToString());
            }
            writer.Close();
        }

        static char GetMap(Position p)
        {
            return map[p.x, p.y];
        }
        static void SetMap(Position p, char c)
        {
            map[p.x, p.y] = c;
        }

        static int xMax = 0;
        static int yMax = 0;
        static int maxDepth = 0;
        static readonly Position goDown = new Position(0, 1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly Position goRight = new Position(1, 0);

        static bool ProcessDownFlow(Position current, int depth)
        {
            if (depth == 0)
            {
                xMax = map.GetLength(0) - 1;
                yMax = map.GetLength(1) - 1;
                maxDepth = 0;
            }
            maxDepth = Math.Max(depth, maxDepth);
            Position below = current + goDown;
            bool reachedDrain = below.y > yMax;
            if (!reachedDrain)
            {
                if (GetMap(below) == '.')
                {
                    SetMap(current, '|');
                    reachedDrain |= ProcessDownFlow(below, depth + 1);
                }
                else if (GetMap(below) == '|')
                {
                    SetMap(current, '|');
                }
                if ((GetMap(below) == '#') || (GetMap(below) == '~'))
                {
                    bool FindStop(ref Position fill, Position direction, ref bool hitDownFlow)
                    {
                        bool hitClay = false;
                        hitDownFlow = false;
                        bool done = false;
                        while (!done)
                        {
                            Position next = fill + direction;
                            Position belowNext = next + goDown;
                            char nextChar = GetMap(next);
                            char belowNextChar = GetMap(belowNext);
                            if (nextChar == '#')
                            {
                                hitClay = true;
                                done = true;
                            }
                            else if ((belowNextChar != '#') && (belowNextChar != '~'))
                            {
                                if (belowNextChar == '.')
                                {
                                    reachedDrain |= ProcessDownFlow(belowNext, depth + 1);
                                }
                                if (GetMap(belowNext) != '~')
                                {
                                    hitDownFlow = true;
                                    done = true;
                                    fill = next;
                                }
                            }
                            else
                            {
                                fill = next;
                            }
                        }
                        return hitClay;
                    }
                    Position left = current;
                    bool toLeftDownFlow = false;
                    bool toLeftWall = FindStop(ref left, goLeft, ref toLeftDownFlow);
                    Position right = current;
                    bool toRightDownFlow = false;
                    bool toRightWall = FindStop(ref right, goRight, ref toRightDownFlow);
                    // Fill
                    char fillChar = toLeftWall && toRightWall ? '~' : '|';
                    if (toLeftWall || toLeftDownFlow)
                    {
                        for (Position p = left; p.x <= current.x; p.x++)
                        {
                            SetMap(p, fillChar);
                        }
                    }
                    if (toRightWall || toRightDownFlow)
                    {
                        for (Position p = current; p.x <= right.x; p.x++)
                        {
                            SetMap(p, fillChar);
                        }
                    }
                }
            }
            else
            {
                SetMap(current, '|');
            }
            return reachedDrain;
        }

        static void PartAB()
        {
            map = ReadInput();
            //PrintMap(map);
            //SaveMap(map, "map_real_initial.txt");
            //SaveMap(map, "map_example_initial.txt");
            Position start = new Position(mapWell.x, mapWell.y + 1);
            ProcessDownFlow(start, 0);
            //SaveMap(map, "map_real_final.txt");
            //SaveMap(map, "map_example_final.txt");
            //PrintMap(map);
            //SaveMap(map, "map_real_initial.txt");
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            int firstRow = -1;
            for (int y = 0; (y < height) && (firstRow < 0); y++)
            {
                for (int x = 0; x < width; x++)
                {
                    if (map[x, y] == '#')
                    {
                        firstRow = y;
                    }
                }
            }
            int water = 0;
            int resting = 0;
            for (int y = firstRow; y < height; y++)
            { 
                for (int x = 0; x < width; x++)
                {
                    if (map[x, y] == '|')
                    {
                        water++;
                    }
                    else if (map[x, y] == '~')
                    {
                        water++;
                        resting++;
                    }
                }
            }
            Console.WriteLine("Part A: Result is " + water + ".");
            Console.WriteLine("Part B: Result is " + resting + ".");
        }


        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day17).Namespace + ":");
            PartAB();
        }
    }
}
