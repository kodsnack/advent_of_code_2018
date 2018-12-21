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

    public class Node
    {
        public string directions;
        public bool optional;
        public List<Node> children;
        public Node()
        {
            children = new List<Node>();
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
        static Map globalMap;

        static Node ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line = reader.ReadLine();
            line = line.Substring(1, line.Length - 2);

            List<string> FindChildren(string s, ref bool optional)
            {
                List<string> children = new List<string>();
                char[] delims = { '(', ')', '|' };
                int i, depth = 0;
                int pos = 0;
                int branchPos = pos;
                while((i = s.IndexOfAny(delims, pos)) > 0)
                {
                    if (s[i] == '|')
                    {
                        if (depth == 0)
                        {
                            string tmp1 = s.Substring(branchPos, i - branchPos);
                            children.Add(tmp1);
                            branchPos = i + 1;
                        }
                    }
                    else if (s[i] == '(')
                    {
                        depth++;
                    }
                    else
                    {
                        depth--;
                    }
                    pos = i + 1;
                }
                optional = (s.Length == branchPos); // Ends in "|)"
                if (!optional)
                {
                    string tmp = s.Substring(branchPos, s.Length - branchPos);
                    children.Add(tmp);
                }
                return children;
            }

            int FindClosingParenthesis(string s, int start)
            {
                char[] delims = { '(', ')' };
                int depth = 0;
                int i = 0;
                int index = start;
                while ((i = s.IndexOfAny(delims, index)) > 0)
                {
                    if (s[i] == '(')
                    {
                        depth++;
                    }
                    else
                    {
                        depth--;
                        if (depth == 0)
                        {
                            return i;
                        }
                    }
                    index = i + 1;
                }
                return -1;
            }

            List<string> SplitIntoSiblings(string input, ref bool firstOptional)
            {
                List<string> nodes = new List<string>();
                char[] delims = { '(', ')', '|' };
                int targetDepth = (input[0] == '(') ? 1 : 0;
                int i, depth = 0;
                int pos = 0;
                int firstPos = pos;
                int lastPos = input.Length - 1;
                while ((i = input.IndexOfAny(delims, pos)) >= 0)
                {
                    if (input[i] == '|')
                    {
                        if (depth == targetDepth)
                        {
                            string tmp1 = input.Substring(firstPos, i - firstPos);
                            nodes.Add(tmp1);
                            firstPos = i + 1;
                        }
                    }
                    else if (input[i] == '(')
                    {
                        depth++;
                        if (depth == targetDepth)
                        {
                            firstPos = i + 1;
                        }
                    }
                    else
                    {
                        depth--;
                        if (depth == targetDepth - 1)
                        {
                            lastPos = i - 1;
                            break;
                        }
                    }
                    pos = i + 1;
                }
                firstOptional = firstPos > lastPos;
                if (firstOptional)
                {
                    string tmp = input.Substring(firstPos + 1);
                    if (tmp.Count(x => x == '(') != tmp.Count(x => x == ')'))
                    {
                        int bug = 1;
                    }
                    nodes.Add(tmp);
                }
                else
                {
                    string tmp = input.Substring(firstPos, lastPos - firstPos + 1);
                    if (tmp.Count(x => x == '(') != tmp.Count(x => x == ')'))
                    {
                        int bug = 1;
                    }
                    nodes.Add(tmp);
                }
                return nodes;
            }

            Node CreateNode(string input)
            {
                Node node = new Node();
                string a = input;
                int p1 = a.IndexOf('(');
                if (p1 > 0)
                {
                    node.directions = a.Substring(0, p1);
                    string b = a.Substring(p1);
                    node.children = ParseInput(b);
                }
                else
                {
                    node.directions = input;
                }
                char[] delims = { '(', ')', '|' };
                if (node.directions.IndexOfAny(delims) >= 0)
                {
                    int bug = 1;
                }
                return node;
            }
            List<Node> ParseInput(string input)
            {
                List<Node> nodes = new List<Node>();
                bool firstOptional = false;
                List<string> nodeStrings = SplitIntoSiblings(input, ref firstOptional);
                foreach (string s in nodeStrings)
                {
                    Node n = CreateNode(s);
                    n.optional = firstOptional;
                    firstOptional = false;
                    nodes.Add(n);
                }
                return nodes;
            }

            Node mainNode = ParseInput(line).First();
            return mainNode;
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

        static Map CreateMap(Node node)
        {
            Position minPos = new Position();
            Position maxPos = new Position();
            Position startPos = new Position();
            void WalkBranch(Node n, Position start, ref Position min, ref Position max)
            {
                Position pos = start;
                foreach (char c in n.directions)
                {
                    pos += tinyDirections[c];
                    if (pos.x < min.x) { min.x = pos.x; }
                    if (pos.y < min.y) { min.y = pos.y; }
                    if (pos.x > max.x) { max.x = pos.x; }
                    if (pos.y > max.y) { max.y = pos.y; }
                }
                foreach (Node child in n.children)
                {
                    WalkBranch(child, pos, ref min, ref max);
                }
            }
            WalkBranch(node, startPos, ref minPos, ref maxPos);
            Position dim = ((maxPos - minPos) + 1) * 2 + 1;
            Position coord = (new Position() - minPos) * 2 + 1;
            Map map = new Map(dim.x, dim.y, coord, '?');
            void FillNeighbors(Position p, Map m)
            {
                m[p] = '.';
                m[p + goUpLeft] = '#';
                m[p + goUpRight] = '#';
                m[p + goDownLeft] = '#';
                m[p + goDownRight] = '#';
            }
            void PopulateMap(Node n, Position p, Map m)
            {
                Position pos = p;
                Position lastPos = pos;
                FillNeighbors(pos, m);
                foreach (char c in n.directions)
                {
                    pos += walkDirections[c];
                    FillNeighbors(pos, m);
                    Position mid = (lastPos + pos) / 2;
                    m[mid] = (mid.x == pos.x) ? '-' : '|';
                    lastPos = pos;
                }
                foreach (Node child in n.children)
                {
                    PopulateMap(child, pos, m);
                }
            }
            PopulateMap(node, map.start, map);
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

        static void PartA()
        {
            Node node = ReadInput();
            globalMap = CreateMap(node);
            PrintMap(globalMap);
            Console.WriteLine("Part A: Result is " + 'A' + ".");
        }

        static void PartB()
        {
            Console.WriteLine("Part B: Result is " + 'B' + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day20).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
