using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day23
{
    public struct Position : IComparable<Position>
    {
        public int x;
        public int y;
        public int z;

        public Position(Position p)
        {
            x = p.x;
            y = p.y;
            z = p.z;
        }
        public Position(int x, int y, int z)
        {
            this.x = x;
            this.y = y;
            this.z = z;
        }
        public int CompareTo(Position p)
        {
            if (x == p.x && y == p.y && z == p.z)
                return 0;
            else
                return (x + y + z < p.x + p.y + p.z) ? -1 : 1;
        }
        public override bool Equals(Object obj)
        {
            return obj is Position && Equals((Position)obj);
        }
        public bool Equals(Position p)
        {
            return (x == p.x) && (y == p.y) && (z == p.z);
        }
        public override int GetHashCode()
        {
            var hashCode = 1502939027;
            hashCode = hashCode * -1521134295 + base.GetHashCode();
            hashCode = hashCode * -1521134295 + x.GetHashCode();
            hashCode = hashCode * -1521134295 + y.GetHashCode();
            hashCode = hashCode * -1521134295 + z.GetHashCode();
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
            return p1 + new Position(k, k, k);
        }
        public static Position operator +(Position p1, Position p2)
        {
            Position p = new Position(p1);
            p.x += p2.x;
            p.y += p2.y;
            p.z += p2.z;
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
            p.z -= p2.z;
            return p;
        }
        public static Position operator *(Position p1, int k)
        {
            Position p = new Position(p1);
            p.x *= k;
            p.y *= k;
            p.z *= k;
            return p;
        }
        public static Position operator /(Position p1, int k)
        {
            Position p = new Position(p1);
            p.x /= k;
            p.y /= k;
            p.z /= k;
            return p;
        }
        public int manhattanDistance(Position p = new Position())
        {
            return Math.Abs(x - p.x) + Math.Abs(y - p.y) + Math.Abs(z - p.z);
        }
    }

    public struct Bot
    {
        public Position pos;
        public int radius;
    }

    class Node
    {
        public Bot bot;
        public HashSet<Node> edges;
        public Node(Bot b)
        {
            bot = b;
            edges = new HashSet<Node>();
        }
    };

    class Cube
    {
        public int bots;
        public Position pos;
        public int side;
        public Cube(Position p, int s)
        {
            bots = 0;
            pos = p;
            side = s;
        }
    };

    class Day23
    {
        static List<Bot> ReadInput()
        {
            List<Bot> bots = new List<Bot>();
            Regex parts = new Regex(@"^.*<([-\d]+),([-\d]+),([-\d]+)>, r=(\d+)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    Bot b = new Bot();
                    int i = 1;
                    b.pos = new Position(int.Parse(groups[i++].Value), int.Parse(groups[i++].Value), int.Parse(groups[i++].Value));
                    b.radius = int.Parse(groups[i++].Value);
                    bots.Add(b);
                }
            }
            return bots;
        }

        static void PartA()
        {
            List<Bot> bots = ReadInput();
            Bot centerBot = bots.OrderBy(x => x.radius).Last();
            int sum = 0;
            foreach (Bot b in bots)
            {
                Position p1 = centerBot.pos;
                Position p2 = b.pos;
                if (Math.Abs(p1.x - p2.x) + Math.Abs(p1.y - p2.y) + Math.Abs(p1.z - p2.z) <= centerBot.radius)
                {
                    sum++;
                }
            }
            Console.WriteLine("Part A: Result is " + sum + ".");
        }

        // --------------------------------------------------------------------
        static HashSet<Node> Union(HashSet<Node> a, HashSet<Node> b)
        {
            HashSet<Node> c = new HashSet<Node>(a);
            c.UnionWith(b);
            return c;
        }

        static HashSet<Node> Intersection(HashSet<Node> a, HashSet<Node> b)
        {
            HashSet<Node> c = new HashSet<Node>(a);
            c.IntersectWith(b);
            return c;
        }

        static HashSet<Node> Difference(HashSet<Node> a, HashSet<Node> b)
        {
            HashSet<Node> c = new HashSet<Node>(a);
            c.ExceptWith(b);
            return c;
        }

        static List<HashSet<Node>> cliques = new List<HashSet<Node>>();

        static void BronKerbosch1(HashSet<Node> R, HashSet<Node> P, HashSet<Node> X)
        {
            if (P.Count == 0 && X.Count == 0)
            {
                cliques.Add(R);
                Console.WriteLine("BK1: Found clique with {0} nodes.", R.Count);
            }
            HashSet<Node> Pc = new HashSet<Node>(P.OrderByDescending(x => x.edges.Count));
            foreach (Node n in Pc)
            {
                HashSet<Node> s = new HashSet<Node>() { n };
                BronKerbosch1(Union(R, s), Intersection(P, n.edges), Intersection(X, n.edges));
                P = Difference(P, s);
                X = Union(X, s);
            }
        }

        static void BronKerbosch2(HashSet<Node> R, HashSet<Node> P, HashSet<Node> X)
        {
            if (P.Count == 0 && X.Count == 0)
            {
                cliques.Add(R);
                //Console.WriteLine("BK2: Found clique with {0} nodes.", R.Count);
            }
            else
            {
                Node pivot = Union(P, X).OrderBy(x => x.edges.Count).Last();
                HashSet<Node> Pu = Difference(P, pivot.edges);
                foreach(Node n in Pu)
                {
                    HashSet<Node> s = new HashSet<Node>() { n };
                    BronKerbosch2(Union(R, s), Intersection(P, n.edges), Intersection(X, n.edges));
                    P = Difference(P, s);
                    X = Union(X, s);
                }
            }
        }

        static void FindBotsInRange(Node a, HashSet<Node> nodes)
        {
            foreach (Node n in nodes)
            {
                if (a.bot.pos.manhattanDistance(n.bot.pos) <= (n.bot.radius + a.bot.radius))
                {
                    a.edges.Add(n);
                }
            }
        }

        static Node MostDistantBot(HashSet<Node> nodes)
        {
            int max = 0;
            Node maxNode = nodes.First();
            foreach (Node n in nodes)
            {
                int d = n.bot.pos.manhattanDistance() - n.bot.radius;
                if (d > max)
                {
                    max = d;
                    maxNode = n;
                }
            }
            return maxNode;
        }

        static void PartB1()
        {
            List<Bot> bots = ReadInput();
            HashSet<Node> nodes = new HashSet<Node>(bots.Select(x => new Node(x)));
            foreach (Node n in nodes)
            {
                HashSet<Node> single = new HashSet<Node>() { n };
                FindBotsInRange(n, Difference(nodes, single));
            }
            List<Node> nodeList = nodes.OrderByDescending(x => x.edges.Count).ToList();
            //int i = 0;
            //foreach (Node n in nodeList)
            //{
            //    Console.WriteLine("Node {0} has {1} connections", i++, n.edges.Count);
            //}
            Console.WriteLine("Part B1: Read {0} nanobots with between {1} and {2} node edges.",
                bots.Count, nodeList.Last().edges.Count, nodeList.First().edges.Count);
            HashSet<Node> R = new HashSet<Node>();
            HashSet<Node> P = new HashSet<Node>(nodes);
            HashSet<Node> X = new HashSet<Node>();
            BronKerbosch2(R, P, X);
            cliques = cliques.OrderByDescending(x => x.Count).ToList();
            HashSet<Node> q = cliques.First();
            int sum = q.Count;
            Console.WriteLine("Part B1: Found {0} cliques, maximal with {1} nodes.", cliques.Count, sum);
            Node bn = MostDistantBot(q);
            bn.edges.Clear();
            HashSet<Node> singleBn = new HashSet<Node>() { bn };
            FindBotsInRange(bn, Difference(q, singleBn));
            Position bnp = bn.bot.pos;
            Console.WriteLine("Part B1: Bot at {0}, {1}, {2} has radius {3} and {4} edges.",
                bnp.x, bnp.y, bnp.z, bn.bot.radius, bn.edges.Count);
            int dist = bnp.manhattanDistance() - bn.bot.radius;
            int sum2 = bn.edges.Count + 1;
            Console.WriteLine("Part B1: Result is " + dist + " (" + sum2 + " bots in range).");
        }

        // --------------------------------------------------------------------
        static int BotsInCube(Position c, int side, List<Bot> bots)
        {
            int sum = 0;
            foreach (Bot b in bots)
            {
                if (IsBotInCube(c, side, b))
                    sum++;
            }
            return sum;
        }

        static readonly List<Position> corners = new List<Position>()
        {
            new Position(0, 0, 0),
            new Position(0, 0, 1),
            new Position(0, 1, 0),
            new Position(0, 1, 1),
            new Position(1, 0, 0),
            new Position(1, 0, 1),
            new Position(1, 1, 0),
            new Position(1, 1, 1),
        };

        static bool IsBotInCube(Position cube, int side, Bot b)
        {
            int offs, dist = 0;
            offs = Math.Max(cube.x - b.pos.x, 0); dist += offs;
            offs = Math.Max(b.pos.x - (cube.x + side - 1), 0); dist += offs;
            offs = Math.Max(cube.y - b.pos.y, 0); dist += offs;
            offs = Math.Max(b.pos.y - (cube.y + side - 1), 0); dist += offs;
            offs = Math.Max(cube.z - b.pos.z, 0); dist += offs;
            offs = Math.Max(b.pos.z - (cube.z + side - 1), 0); dist += offs;
            return dist <= b.radius;
        }

        static int CubeMinOrigoDistance(Position c, int side)
        {
            Position p = new Position();
            int SmallestAbs(int a, int b)
            {
                return (Math.Abs(a) < Math.Abs(b)) ? a : b;
            }
            p.x = SmallestAbs(c.x, c.x + side - 1);
            p.y = SmallestAbs(c.y, c.y + side - 1);
            p.z = SmallestAbs(c.z, c.z + side - 1);
            return p.manhattanDistance();
        }

        static void PartB2()
        {
            List<Bot> bots = ReadInput();
            int dMax = bots.Select(x => x.pos.manhattanDistance()).OrderBy(x => x).Last();
            int step = 1;
            while (step < dMax)
                step *= 2;
            Cube current = new Cube(new Position(-step, -step, -step), step * 2);
            current.bots = BotsInCube(current.pos, current.side, bots);
            List<Cube> workQueue = new List<Cube>() { current };
            int lastSide = int.MaxValue;
            do {
                workQueue = workQueue.
                    OrderByDescending(x => x.bots).
                    ThenBy(x => CubeMinOrigoDistance(x.pos, x.side)).
                    ThenBy(x => x.side).ToList();
                current = workQueue.First();
                workQueue.RemoveAt(0);
                if (current.side > 1)
                {
                    int nextSide = current.side / 2;
                    foreach (Position corner in corners)
                    {
                        Cube part = new Cube(current.pos + corner * nextSide, nextSide);
                        part.bots = BotsInCube(part.pos, part.side, bots);
                        if (part.bots > 0)
                            workQueue.Add(part);
                    }
                    if (nextSide < lastSide)
                    {
                        //Console.WriteLine("Part B2: Zooming into cubes with size {0} ({1} bots, {2} cubes in queue)", 
                        //    nextSide, current.bots, workQueue.Count);
                        lastSide = nextSide;
                    }
                }
            }
            while (current.side > 1);
            Position bp = current.pos;
            int dist = bp.manhattanDistance();
            Console.WriteLine("Part B2: Result is {0} ({1} bots, position: {2}, {3}, {4}).",
                dist, current.bots, bp.x, bp.y, bp.z);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day23).Namespace + ":");
            PartA();
            PartB1();
            PartB2();
        }
    }
}
