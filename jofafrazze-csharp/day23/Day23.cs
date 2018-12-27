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
    }

    public struct Bot
    {
        public Position pos;
        public int radius;
        //public Bot(Position p, int r)
        //{
        //    pos = p;
        //    radius = r;
        //}
    }

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

        static int CalculateInRange(List<Bot> bots, Bot ourBot)
        {
            int sum = 0;
            foreach (Bot b in bots)
            {
                Position p1 = ourBot.pos;
                Position p2 = b.pos;
                if (Math.Abs(p1.x - p2.x) + Math.Abs(p1.y - p2.y) + Math.Abs(p1.z - p2.z) <= (b.radius + ourBot.radius))
                {
                    sum++;
                }
            }
            return sum;
        }

        static void PartB()
        {
            List<Bot> bots = ReadInput();
            SortedList<int, List<Bot>> botsWithFriends = new SortedList<int, List<Bot>>();
            foreach (Bot b in bots)
            {
                int n = CalculateInRange(bots, b);
                if (!botsWithFriends.ContainsKey(n))
                    botsWithFriends.Add(n, new List<Bot>());
                botsWithFriends[n].Add(b);
            }
            foreach (KeyValuePair<int,List<Bot>> kvp in botsWithFriends)
            {
                Console.Write("Part B: Reaching {0} bots, {1} different bots. r = ", kvp.Key, kvp.Value.Count());
                //foreach (Bot b in kvp.Value)
                //    Console.Write(b.radius.ToString() + ", ");
                Console.WriteLine("");
            }
            //Console.WriteLine("Part B: Result is " + dist + " (" + sum + " bots in range).");
        }

        //static void PartB()
        //{
        //    List<Bot> bots = ReadInput();
        //    Bot b = new Bot();
        //    int nMax = 0;
        //    for (int i = 0; i < 1000000000; i++)
        //    {
        //        b.radius = i;
        //        int n = CalculateInRange(bots, b);
        //        if (n > nMax)
        //        {
        //            nMax = n;
        //            Console.WriteLine("Part B: New max is " + i + " (" + n + " bots in range).");
        //        }
        //    }
        //    //Console.WriteLine("Part B: Result is " + dist + " (" + sum + " bots in range).");
        //}

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day23).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
