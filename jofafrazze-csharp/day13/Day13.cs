using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace day13
{
    class Day13
    {
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

        public struct Position
        {
            public int x;
            public int y;

            public Position(int x, int y)
            {
                this.x = x;
                this.y = y;
            }
            public bool Equals(Position p)
            {
                return (x == p.x) && (y == p.y);
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

        // 0 = left, 1 = straight, 2 = right
        static public Dictionary<Position, int> carts = new Dictionary<Position, int>();

        static public readonly Dictionary<char, char> turnLeft = new Dictionary<char, char>()
        {
            { '^', '<' }, { '>', '^' }, { 'v', '>' }, { '<', 'v' }
        };
        static public readonly Dictionary<char, char> turnRight = new Dictionary<char, char>()
        {
            { '^', '>' }, { '>', 'v' }, { 'v', '<' }, { '<', '^' }
        };
        static public readonly Dictionary<char, char> hitVertical = new Dictionary<char, char>()
        {
            { '^', '^' }, { '>', '*' }, { 'v', 'v' }, { '<', '*' }
        };
        static public readonly Dictionary<char, char> hitUpRight = new Dictionary<char, char>()
        {
            { '^', '>' }, { '>', '^' }, { 'v', '<' }, { '<', 'v' }
        };
        static public readonly Dictionary<char, char> hitHorizontal = new Dictionary<char, char>()
        {
            { '^', '*' }, { '>', '>' }, { 'v', '*' }, { '<', '<' }
        };
        static public readonly Dictionary<char, char> hitDownRight = new Dictionary<char, char>()
        {
            { '^', '<' }, { '>', 'v' }, { 'v', '>' }, { '<', '^' }
        };

        static public readonly List<char> cartChars = new List<char>() { '^', '>', 'v', '<' };

        static void PopulateCarts(char[,] map)
        {
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            for (int x = 0; x < width; x++)
            {
                for (int y = 0; y < height; y++)
                {
                    char c = map[x, y];
                    if (cartChars.Contains(c))
                    {
                        Position p = new Position(x, y);
                        carts[p] = 0;
                    }
                }
            }
        }

        static public readonly List<char> canGoVertical = new List<char>() { '\\', '|', '/', '+'};
        static public readonly List<char> canGoHorizontal = new List<char>() { '\\', '-', '/', '+' };

        static void CleanCartPosition(ref char[,] m, int x, int y)
        {
            int width = m.GetLength(0);
            int height = m.GetLength(1);
            char c = m[x, y];
            char cReplace = c;
            if (cartChars.Contains(c))
            {
                char cu = (y > 0) ? m[x, y - 1] : '*';
                char cr = (x < width - 1) ? m[x + 1, y] : '*';
                char cd = (y < height - 1) ? m[x, y + 1] : '*';
                char cl = (x > 0) ? m[x - 1, y] : '*';
                bool bu = canGoVertical.Contains(cu);
                bool br = canGoHorizontal.Contains(cr);
                bool bd = canGoVertical.Contains(cd);
                bool bl = canGoHorizontal.Contains(cl);
                if (bu && br && bd && bl)
                {
                    cReplace = '+';
                }
                else if ((bu && br) || (bd && bl))
                {
                    cReplace = '\\';
                }
                else if ((br && bd) || (bl && bu))
                {
                    cReplace = '/';
                }
                else if (bu && bd)
                {
                    cReplace = '|';
                }
                else if (br && bl)
                {
                    cReplace = '-';
                }
            }
            m[x, y] = cReplace;
        }

        static char[,] CleanCarts(char[,] map)
        {
            char[,] m = (char[,])map.Clone();
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            for (int x = 0; x < width; x++)
            {
                for (int y = 0; y < height; y++)
                {
                    CleanCartPosition(ref m, x, y);
                }
            }
            return m;
        }

        static void PrintMap(char[,] map)
        {
            Console.Clear();
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
            Console.WriteLine();
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

        static public readonly List<char> roadChars = new List<char>() { '|', '/', '-', '\\', '+' };

        static Position StepMap(ref char[,] map, ref char[,] nextMap, char[,] cleanMap, Position curPos, ref bool crashed)
        {
            Position newPos = curPos;
            char c = map[curPos.x, curPos.y];
            if (c == '^')
            {
                newPos.y = curPos.y - 1;
            }
            else if (c == '>')
            {
                newPos.x = curPos.x + 1;
            }
            else if (c == 'v')
            {
                newPos.y = curPos.y + 1;
            }
            else if (c == '<')
            {
                newPos.x = curPos.x - 1;
            }
            if (newPos != curPos)
            {
                int nextTurn = carts[curPos];
                carts.Remove(curPos);
                char r = cleanMap[newPos.x, newPos.y];
                char cNext = '*';
                if (r == '|')
                {
                    cNext = hitVertical[c];
                }
                else if (r == '/')
                {
                    cNext = hitUpRight[c];
                }
                else if (r == '-')
                {
                    cNext = hitHorizontal[c];
                }
                else if (r == '\\')
                {
                    cNext = hitDownRight[c];
                }
                else if (r == '+')
                {
                    if (nextTurn == 0)
                    {
                        cNext = turnLeft[c];
                        nextTurn = 1;
                    }
                    else if (nextTurn == 1)
                    {
                        cNext = c;
                        nextTurn = 2;
                    }
                    else
                    {
                        cNext = turnRight[c];
                        nextTurn = 0;
                    }
                }
                char rNext = nextMap[newPos.x, newPos.y];
                if (!roadChars.Contains(rNext))
                {
                    crashed = true;
                }
                carts[newPos] = nextTurn;
                nextMap[curPos.x, curPos.y] = cleanMap[curPos.x, curPos.y];
                nextMap[newPos.x, newPos.y] = cNext;
                if ((cNext == '*') && !crashed)
                {
                    int bug = 1;
                }
            }
            return newPos;
        }

        static int CountCarts(char[,] map, ref Position pos)
        {
            int n = 0;
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            for (int x = 0; x < width; x++)
            {
                for (int y = 0; y < height; y++)
                {
                    if (cartChars.Contains(map[x, y]))
                    {
                        n++;
                        pos = new Position(x, y);
                    }
                }
            }
            return n;
        }

        static void PartA()
        {
            char[,] map = ReadInput();
            //PrintMap(map);
            PopulateCarts(map);
            char[,] cleanMap = CleanCarts(map);
            //PrintMap(cleanMap);
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            Position curPos = new Position();
            Position newPos = new Position();
            bool crashed = false;
            int tick = 0;
            do
            {
                char[,] nextMap = (char[,])map.Clone();
                for (int y = 0; (y < height) && !crashed; y++)
                {
                    curPos.y = y;
                    for (int x = 0; (x < width) && !crashed; x++)
                    {
                        curPos.x = x;
                        newPos = StepMap(ref map, ref nextMap, cleanMap, curPos, ref crashed);
                    }
                }
                map = (char[,])nextMap.Clone();
                //PrintMap(map);
                tick++;
            }
            while (!crashed);
            Console.WriteLine("Part A: Result is " + newPos.x + ", " + newPos.y + " (took " + tick + " ticks).");
        }

        static void PartB()
        {
            char[,] map = ReadInput();
            //SaveMap(map, "map_b.txt");
            PopulateCarts(map);
            char[,] cleanMap = CleanCarts(map);
            //SaveMap(cleanMap, "map_b_clean.txt");
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            Position curPos = new Position();
            Position newPos = new Position();
            bool crashed = false;
            int tick = 0;
            int nCarts = 0;
            do
            {
                if (tick < 50)
                {
                    //SaveMap(map, "map_tick_" + tick.ToString("D2") + ".txt");
                }
                char[,] nextMap = (char[,])map.Clone();
                for (int y = 0; (y < height) && !crashed; y++)
                {
                    curPos.y = y;
                    for (int x = 0; (x < width) && !crashed; x++)
                    {
                        curPos.x = x;
                        newPos = StepMap(ref map, ref nextMap, cleanMap, curPos, ref crashed);
                        if (crashed)
                        {
                            nextMap[newPos.x, newPos.y] = cleanMap[newPos.x, newPos.y];
                            crashed = false;
                        }
                    }
                }
                map = (char[,])nextMap.Clone();
                nCarts = CountCarts(map, ref newPos);
                //PrintMap(map);
                //Thread.Sleep(200);
                tick++;
            }
            while (nCarts > 1);
            Console.WriteLine("Part B: WRONG Result is " + newPos.x + ", " + newPos.y + " (took " + tick + " ticks).");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day13).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
