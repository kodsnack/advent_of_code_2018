using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day25
{
    public struct Position : IComparable<Position>
    {
        public int x;
        public int y;
        public int z;
        public int w;

        public Position(Position p)
        {
            x = p.x;
            y = p.y;
            z = p.z;
            w = p.w;
        }
        public Position(int x, int y, int z, int w)
        {
            this.x = x;
            this.y = y;
            this.z = z;
            this.w = w;
        }
        public int CompareTo(Position p)
        {
            if (x == p.x && y == p.y && z == p.z && w == p.w)
                return 0;
            else
                return (x + y + z + w < p.x + p.y + p.z + p.w) ? -1 : 1;
        }
        public override bool Equals(Object obj)
        {
            return obj is Position && Equals((Position)obj);
        }
        public bool Equals(Position p)
        {
            return (x == p.x) && (y == p.y) && (z == p.z) && (w == p.w);
        }
        public override int GetHashCode()
        {
            var hashCode = 1502939027;
            hashCode = hashCode * -1521134295 + base.GetHashCode();
            hashCode = hashCode * -1521134295 + x.GetHashCode();
            hashCode = hashCode * -1521134295 + y.GetHashCode();
            hashCode = hashCode * -1521134295 + z.GetHashCode();
            hashCode = hashCode * -1521134295 + w.GetHashCode();
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
            return p1 + new Position(k, k, k, k);
        }
        public static Position operator +(Position p1, Position p2)
        {
            Position p = new Position(p1);
            p.x += p2.x;
            p.y += p2.y;
            p.z += p2.z;
            p.w += p2.w;
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
            p.w -= p2.w;
            return p;
        }
        public static Position operator *(Position p1, int k)
        {
            Position p = new Position(p1);
            p.x *= k;
            p.y *= k;
            p.z *= k;
            p.w *= k;
            return p;
        }
        public static Position operator /(Position p1, int k)
        {
            Position p = new Position(p1);
            p.x /= k;
            p.y /= k;
            p.z /= k;
            p.w /= k;
            return p;
        }
        public static int manhattanDistance(Position p1, Position p2)
        {
            return Math.Abs(p1.x - p2.x) + Math.Abs(p1.y - p2.y) + Math.Abs(p1.z - p2.z) + Math.Abs(p1.w - p2.w);
        }
    }

    class Day25
    {
        static List<Position> ReadInput()
        {
            List<Position> list = new List<Position>();
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                int[] d = line.Split(',').Select(int.Parse).ToArray();
                Position p = new Position(d[0], d[1], d[2], d[3]);
                list.Add(p);
            }
            return list;
        }

        static void PartA()
        {
            List<Position> positions = ReadInput();
            List<Position> constellation = ReadInput();
            int total = positions.Count;
            int sum = 0;
            while (positions.Count() > 0)
            {
                constellation.Clear();
                constellation.Add(positions[0]);
                positions.RemoveAt(0);
                bool done = false;
                do
                {
                    done = false;
                    bool isPart = false;
                    int index = -1;
                    for (int i = 0; (i < constellation.Count) && !isPart; i++)
                    {
                        for (int n = 0; (n < positions.Count) && !isPart; n++)
                        {
                            if (Position.manhattanDistance(constellation[i], positions[n]) <= 3)
                            {
                                isPart = true;
                                index = n;
                            }
                        }
                        if ((i == constellation.Count - 1) && !isPart)
                        {
                            done = true;
                        }
                    }
                    if (index >= 0)
                    {
                        constellation.Add(positions[index]);
                        positions.RemoveAt(index);
                    }
                }
                while (!done);
                sum++;
                int left = positions.Count;
                Console.WriteLine("({0}/{1}) Added {2} positions to a new constellation.", 
                    total - left, total, constellation.Count);
            }
            Console.WriteLine("Part A: Result is " + sum + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day25).Namespace + ":");
            PartA();
        }
    }
}
