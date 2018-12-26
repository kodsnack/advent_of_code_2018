using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day20
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
        public static Position operator +(Position p1, int k)
        {
            return p1 + new Position(k, k);
        }
        public static Position operator +(Position p1, Position p2)
        {
            Position p = new Position(p1);
            p.x += p2.x;
            p.y += p2.y;
            return p;
        }
        public static Position operator -(Position p1, int k)
        {
            return p1 + (-k);
        }
        public static Position operator -(Position p1, Position p2)
        {
            Position p = new Position(p1);
            p.x -= p2.x;
            p.y -= p2.y;
            return p;
        }
        public static Position operator *(Position p1, int k)
        {
            Position p = new Position(p1);
            p.x *= k;
            p.y *= k;
            return p;
        }
        public static Position operator /(Position p1, int k)
        {
            Position p = new Position(p1);
            p.x /= k;
            p.y /= k;
            return p;
        }
    }

    public class Map
    {
        public int width;
        public int height;
        public Position start;
        public char[,] map;

        public Map(int w, int h, Position s, char fill = '\0')
        {
            width = w;
            height = h;
            start = s;
            map = new char[w, h];
            for (int i = 0; i < w * h; i++)
            {
                map[i % w, i / w] = fill;
            }
        }

        public char this[Position p]
        {
            get
            {
                return map[p.x, p.y];
            }
            set
            {
                map[p.x, p.y] = value;
            }
        }
    }

    class Day20
    {
        static string ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line = reader.ReadLine();
            return line.Substring(1, line.Length - 2);
        }

        static void PrintMap(Map m)
        {
            for (int y = 0; y < m.height; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < m.width; x++)
                {
                    sb.Append(m.map[x, y]);
                }
                Console.WriteLine(sb.ToString());
            }
        }

        static readonly Position goUp = new Position(0, -1);
        static readonly Position goLeft = new Position(-1, 0);
        static readonly Position goRight = new Position(1, 0);
        static readonly Position goDown = new Position(0, 1);

        static readonly Position goUpLeft = goUp + goLeft;
        static readonly Position goUpRight = goUp + goRight;
        static readonly Position goDownLeft = goDown + goLeft;
        static readonly Position goDownRight = goDown + goRight;

        static readonly Dictionary<char, Position> tinyDirections = new Dictionary<char, Position>()
        {
            { 'N', goUp },
            { 'E', goRight },
            { 'S', goDown },
            { 'W', goLeft },
        };

        static readonly Dictionary<char, Position> walkDirections = new Dictionary<char, Position>()
        {
            { 'N', goUp * 2 },
            { 'E', goRight * 2 },
            { 'S', goDown * 2 },
            { 'W', goLeft * 2 },
        };

        static Map CreateMap(string regex)
        {
            Position min = new Position();
            Position max = new Position();
            Position pos = new Position();
            Stack<Position> stack = new Stack<Position>();
            foreach (char c in regex)
            {
                if (c == '(')       { stack.Push(pos); }
                else if (c == '|')  { pos = stack.Peek(); }
                else if (c == ')')  { pos = stack.Pop(); }
                else
                {
                    pos += tinyDirections[c];
                    if (pos.x < min.x) { min.x = pos.x; }
                    if (pos.y < min.y) { min.y = pos.y; }
                    if (pos.x > max.x) { max.x = pos.x; }
                    if (pos.y > max.y) { max.y = pos.y; }
                }
            }
            Position dim = ((max - min) + 1) * 2 + 1;
            Position coord = (new Position() - min) * 2 + 1;
            Map map = new Map(dim.x, dim.y, coord, '?');
            void FillNeighbors(Position p, Map m)
            {
                m[p] = '.';
                m[p + goUpLeft] = '#';
                m[p + goUpRight] = '#';
                m[p + goDownLeft] = '#';
                m[p + goDownRight] = '#';
            }
            pos = coord;
            Position lastPos;
            FillNeighbors(pos, map);
            foreach (char c in regex)
            {
                if (c == '(') { stack.Push(pos); }
                else if (c == '|') { pos = stack.Peek(); }
                else if (c == ')') { pos = stack.Pop(); }
                else
                {
                    lastPos = pos;
                    pos += walkDirections[c];
                    FillNeighbors(pos, map);
                    Position mid = (lastPos + pos) / 2;
                    map[mid] = (mid.x == pos.x) ? '-' : '|';
                }
            }
            map.map[map.start.x, map.start.y] = 'X';
            for (int i = 0; i < map.width * map.height; i++)
            {
                if (map.map[i % map.width, i / map.width] == '?')
                {
                    map.map[i % map.width, i / map.width] = '#';
                }
            }
            return map;
        }

        static Tuple<int, int> WalkTheMap(string regex)
        {
            Position pos = new Position();
            HashSet<Position> visited = new HashSet<Position>();
            Stack<Position> spos = new Stack<Position>();
            Stack<int> sdoors = new Stack<int>();
            int doors = 0;
            int maxDoors = 0;
            int distantRooms = 0;
            foreach (char c in regex)
            {
                if (c == '(') { spos.Push(pos); sdoors.Push(doors); }
                else if (c == '|') { pos = spos.Peek(); doors = sdoors.Peek(); }
                else if (c == ')') { pos = spos.Pop(); doors = sdoors.Pop(); }
                else
                {
                    pos += tinyDirections[c];
                    if (!visited.Contains(pos))
                    {
                        doors++;
                        maxDoors = Math.Max(maxDoors, doors);
                        if (doors >= 1000)
                        {
                            distantRooms++;
                        }
                        visited.Add(pos);
                    }
                }
            }
            return Tuple.Create(maxDoors, distantRooms);
        }

        static void PartAB()
        {
            string regex = ReadInput();
            Map map = CreateMap(regex);
            //PrintMap(map);
            Tuple<int, int> answer = WalkTheMap(regex);
            Console.WriteLine("Part A: Result is " + answer.Item1 + ".");
            Console.WriteLine("Part B: Result is " + answer.Item2 + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day20).Namespace + ":");
            PartAB();
        }
    }
}
