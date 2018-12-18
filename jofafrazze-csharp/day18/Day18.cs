using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace day18
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

    public struct Inhabitants
    {
        public int open;
        public int tree;
        public int lumber;

        public bool Equals(Inhabitants i)
        {
            return (open == i.open) && (tree == i.tree) && (lumber == i.lumber);
        }
    }

    class Day18
    {
        static char[,] map;

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

        static readonly char open = '.';
        static readonly char tree = '|';
        static readonly char lumber = '#';

        static void UpdateNeighbors(char c, ref Inhabitants neighbors)
        {
            if (c == open)
            {
                neighbors.open++;
            }
            else if (c == tree)
            {
                neighbors.tree++;
            }
            else if (c == lumber)
            {
                neighbors.lumber++;
            }
        }

        static Inhabitants CalculateNeighbors(Position p)
        {
            Inhabitants neighbors = new Inhabitants();
            int xMax = map.GetLength(0) - 1;
            int yMax = map.GetLength(1) - 1;
            for (int x = p.x - 1; x <= p.x + 1; x++)
            {
                if ((x >= 0) && (x <= xMax))
                {
                    if (p.y - 1 >= 0)
                    {
                        UpdateNeighbors(map[x, p.y - 1], ref neighbors);
                    }
                    if (p.y + 1 <= yMax)
                    {
                        UpdateNeighbors(map[x, p.y + 1], ref neighbors);
                    }
                }
            }
            if (p.x - 1 >= 0)
            {
                UpdateNeighbors(map[p.x - 1, p.y], ref neighbors);
            }
            if (p.x + 1 <= xMax)
            {
                UpdateNeighbors(map[p.x + 1, p.y], ref neighbors);
            }
            return neighbors;
        }

        static void IterateMap()
        {
            char[,] nextMap = (char[,])map.Clone();
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            Position p = new Position();
            for (p.y = 0; p.y < height; p.y++)
            {
                for (p.x = 0; p.x < width; p.x++)
                {
                    Inhabitants n = CalculateNeighbors(p);
                    if (map[p.x, p.y] == open)
                    {
                        nextMap[p.x, p.y] = (n.tree >= 3) ? tree : open;
                    }
                    else if (map[p.x, p.y] == tree)
                    {
                        nextMap[p.x, p.y] = (n.lumber >= 3) ? lumber : tree;
                    }
                    else if (map[p.x, p.y] == lumber)
                    {
                        nextMap[p.x, p.y] = ((n.lumber >= 1) && (n.tree >= 1)) ? lumber : open;
                    }
                }
            }
            map = nextMap;
        }

        static Inhabitants CalculateTotals(char[,] m)
        {
            Inhabitants totals = new Inhabitants();
            int width = m.GetLength(0);
            int height = m.GetLength(1);
            for (int y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    char c = m[x, y];
                    if (c == open)
                    {
                        totals.open++;
                    }
                    else if (c == tree)
                    {
                        totals.tree++;
                    }
                    else if (c == lumber)
                    {
                        totals.lumber++;
                    }
                }
            }
            return totals;
        }

        static void PartA()
        {
            map = ReadInput();
            //PrintMap(map);
            //Console.WriteLine();
            int iMax = 10;
            for (int i = 0; i < iMax; i++)
            {
                IterateMap();
            }
            //PrintMap(map);
            Inhabitants totals = CalculateTotals(map);
            int value = totals.tree * totals.lumber;
            Console.WriteLine("Part A: Result is " + value + ".");
        }

        static bool MapsEqual(char[,] a, char[,] b)
        {
            if (a.GetLength(0) != b.GetLength(0) || a.GetLength(1) != b.GetLength(1))
            {
                return false;
            }
            int width = a.GetLength(0);
            int height = a.GetLength(1);
            for (int y = 0; y < height; y++)
            {
                for (int x = 0; x < width; x++)
                {
                    if (a[x, y] != b[x,y])
                    {
                        return false;
                    }
                }
            }
            return true;
        }

        static List<Inhabitants> mapTotals = new List<Inhabitants>();
        static List<char[,]> maps = new List<char[,]>();

        static int PreviousEqualMap()
        {
            Inhabitants a = CalculateTotals(map);
            if (mapTotals.Count > 0)
            {
                for (int i = 0; i < mapTotals.Count; i++)
                {
                    if (a.Equals(mapTotals[i]))
                    {
                        if (MapsEqual(map, maps[i]))
                        {
                            return i;
                        }
                    }
                }
            }
            mapTotals.Add(a);
            maps.Add(map);
            return -1;
        }

        static void PartB()
        {
            map = ReadInput();
            //PrintMap(map);
            //Console.WriteLine();
            char[,] lastMap;
            int iMax = 1000000000;
            bool done = false;
            int iter = 0;
            int indexFound = -1;
            for (int i = 0; (i < iMax) && !done; i++)
            {
                //Console.Clear();
                //PrintMap(map);
                //Thread.Sleep(100);
                if ((indexFound = PreviousEqualMap()) > 0)
                {
                    done = true;
                    iter = i;
                }
                else
                {
                    IterateMap();
                }
            }
            int offs = indexFound;
            int cycle = iter - indexFound;
            int cOffs = (iMax - offs) % cycle;
            int iMaxIndex = offs + cOffs;
            Inhabitants totals = CalculateTotals(maps[iMaxIndex]);
            int value = totals.tree * totals.lumber;
            Console.WriteLine("Part B: Result is " + value + " (took " + iter + " iterations).");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day18).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
