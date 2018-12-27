using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day22
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

    class Day22
    {
        static void PrintMap(Map m, Position target)
        {
            char[] types = { '.', '=', '|' };
            for (int y = 0; y < m.height; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < m.width; x++)
                {
                    sb.Append(types[m.map[x, y]]);
                }
                if (y == 0)
                    sb[0] = 'M';
                else if (y == target.y)
                    sb[target.x] = 'T';
                Console.WriteLine(sb.ToString());
            }
        }

        static Map CreateMap(int depth, Position target, int mapExpansion)
        {
            const int m = 20183;
            int Erosion(int gi)
            {
                return (gi + depth) % m;
            }
            Position origo = new Position();
            int w = target.x + 1 + mapExpansion;
            int h = target.y + 1 + mapExpansion;
            Map type = new Map(w, h, origo);
            int[,] geo = new int[w, h];
            geo[0, 0] = 0;
            for (int x = 1; x < w; x++)
            {
                geo[x, 0] = (x * 16807) % m;
            }
            for (int y = 1; y < h; y++)
            {
                geo[0, y] = (y * 48271) % m;
            }
            for (int x = 1; x < w; x++)
            {
                for (int y = 1; y < h; y++)
                {
                    if ((x == target.x) && (y == target.y))
                        geo[x, y] = 0;
                    else
                        geo[x, y] = (Erosion(geo[x - 1, y]) * Erosion(geo[x, y - 1])) % m;
                }
            }
            for (int x = 0; x < w; x++)
            {
                for (int y = 0; y < h; y++)
                {
                    type.map[x, y] = (char)(((geo[x, y] + depth) % m) % 3);
                }
            }
            return type;
        }

        static int SumToTarget(Map map, Position target)
        {
            int w = target.x + 1;
            int h = target.y + 1;
            int sum = 0;
            for (int i = 0; i < w * h; i++)
            {
                sum += map.map[i % w, i / w];
            }
            return sum;
        }

        static readonly Position goUp = new Position(0, -1, 0);
        static readonly Position goLeft = new Position(-1, 0, 0);
        static readonly Position goRight = new Position(1, 0, 0);
        static readonly Position goDown = new Position(0, 1, 0);
        static readonly Position goUpZ = new Position(0, 0, 1);
        static readonly Position goDownZ = new Position(0, 0, -1);
        static readonly List<Position> alternatives = new List<Position>()
        {
            goUp, goLeft, goRight, goDown, goUpZ, goDownZ,
        };

        // z: 0 = none, 1 = torch, 2 = climbing gear
        // regions: 0 = rocky, 1 = wet, 2 = narrow
        static int CalcFastestPath(Map map, Position target)
        {
            bool InMap(Position p)
            {
                return p.x >= 0 && p.x < map.width && p.y >= 0 && p.y < map.height;
            }
            // z: 0 = torch, 1 = climbing gear, 2 = none
            List<Position> toCheck = new List<Position>();
            List<Position> toCheckNext = new List<Position>();
            Dictionary<Position, int> progress = new Dictionary<Position, int>();
            void SetProgress(Position p, int minutes)
            {
                progress[p] = minutes;
                toCheckNext.Add(p);
            }
            SetProgress(new Position(0, 0, 1), 0);
            do
            {
                foreach (Position p in toCheck)
                {
                    foreach (Position a in alternatives)
                    {
                        Position t = p + a;
                        t.z = (t.z + 3) % 3;
                        if (InMap(t))
                        {
                            if (map[t] != t.z)
                            {
                                int minutes = (p.z == t.z) ? 1 : 7;
                                if (!progress.ContainsKey(t) || (progress[t] > progress[p] + minutes))
                                {
                                    SetProgress(t, progress[p] + minutes);
                                }
                            }
                        }
                    }
                }
                toCheck = toCheckNext;
                toCheckNext = new List<Position>();
            }
            while (toCheck.Count > 0);

            // Debug
/*
            List<Position> examplePath = new List<Position>()
            {
                goDown, goRight, goDownZ,
                goRight, goRight, goRight, goDownZ,
                goDown, goDown, goDown, goDown, goDown, goDown, goDown,
                goRight, goDown, goDown, goDown,
                goRight, goDown, goRight, goRight, goRight, goRight,
                goUp, goUp, goDownZ
            };
            Position pos = new Position(0, 0, 1);
            int lastMinutes = 0;
            foreach (Position a in examplePath)
            {
                pos += a;
                pos.z = (pos.z + 3) % 3;
                int minutes = progress[pos];
                Console.WriteLine("Got to {0}, {1}, {2} in {3} minutes ({4} total)", 
                    pos.x, pos.y, pos.z, minutes - lastMinutes, minutes);
                lastMinutes = minutes;
            }
*/
            return progress[target];
        }

        static void PartAB()
        {
            //int depth = 510;
            //Position target = new Position(10, 10, 1);
            //Map map = CreateMap(depth, target, 5);
            int depth = 4002;
            Position target = new Position(5, 746, 1);
            Map map = CreateMap(depth, target, 50);
            PrintMap(map, target);
            int sum = SumToTarget(map, target);
            Console.WriteLine("Part A: Result is " + sum + ".");
            int minutes = CalcFastestPath(map, target);
            Console.WriteLine("Part B: Result is " + minutes + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day22).Namespace + ":");
            PartAB();
        }
    }
}
