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

        public struct Cart
        {
            public Position pos;
            public int heading;     // 0 = up, 1 = right, 2 = down, 3 = left
            public int nextTurn;    // 0 = left, 1 = straight, 2 = right
            public bool moved;
            public bool crashed;

            public Cart(Position p, int direction)
            {
                pos = p;
                heading = direction;
                nextTurn = 0;
                moved = false;
                crashed = false;
            }
            public Cart(Cart c)
            {
                pos = c.pos;
                heading = c.heading;
                nextTurn = c.nextTurn;
                moved = c.moved;
                crashed = c.crashed;
            }
            public Cart(Cart c, bool crashedDirectly)
            {
                this = new Cart(c);
                crashed = crashedDirectly;
            }
        }

        static public List<Cart> carts = new List<Cart>();
        static public Dictionary<Position, int> cartIndexes = new Dictionary<Position, int>();

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
        static public readonly List<char> roadChars = new List<char>() { '|', '/', '-', '\\', '+' };

        static public readonly List<char> canGoVertical = new List<char>() { '\\', '|', '/', '+' };
        static public readonly List<char> canGoHorizontal = new List<char>() { '\\', '-', '/', '+' };

        static void CreateCarts(char[,] map)
        {
            carts.Clear();
            cartIndexes.Clear();
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
                        Cart cart = new Cart(p, cartChars.IndexOf(c));
                        carts.Add(cart);
                        cartIndexes[p] = carts.Count - 1;
                    }
                }
            }
        }

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

        static void PrintMap(char[,] map, List<Cart> carts)
        {
            char[,] printMap = (char[,])map.Clone();
            foreach (Cart c in carts)
            {
                printMap[c.pos.x, c.pos.y] = cartChars[c.heading];
            }
            PrintMap(printMap);
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

        static Cart MoveCart(Cart cart, ref char[,] map)
        {
            Cart movedCart = new Cart(cart);
            movedCart.moved = true;
            switch (cart.heading)
            {
                case 0: movedCart.pos.y -= 1; break;
                case 1: movedCart.pos.x += 1; break;
                case 2: movedCart.pos.y += 1; break;
                case 3: movedCart.pos.x -= 1; break;
            }
            char c = cartChars[cart.heading];
            char r = map[movedCart.pos.x, movedCart.pos.y];
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
                if (movedCart.nextTurn == 0)
                {
                    cNext = turnLeft[c];
                    movedCart.nextTurn = 1;
                }
                else if (movedCart.nextTurn == 1)
                {
                    cNext = c;
                    movedCart.nextTurn = 2;
                }
                else
                {
                    cNext = turnRight[c];
                    movedCart.nextTurn = 0;
                }
            }
            if (!cartChars.Contains(cNext))
            {
                int bug = 1;
            }
            movedCart.heading = cartChars.IndexOf(cNext);
            return movedCart;
        }

        static bool FindNewCrashes(ref Position pos)
        {
            bool newCrashes = false;
            for (int i = 0; i < carts.Count; i++)
            {
                if (!carts[i].crashed)
                {
                    for (int n = 0; n < carts.Count; n++)
                    {
                        if ((i != n) && (carts[i].pos == carts[n].pos))
                        {
                            pos = carts[i].pos;
                            carts[i] = new Cart(carts[i], true);
                            carts[n] = new Cart(carts[n], true);
                            newCrashes = true;
                        }
                    }
                }
            }
            return newCrashes;
        }

        static void PartA()
        {
            char[,] map = ReadInput();
            //PrintMap(map);
            CreateCarts(map);
            char[,] cleanMap = CleanCarts(map);
            //PrintMap(cleanMap);
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            Position pos = new Position();
            Position crashPos = new Position();
            bool crashed = false;
            int tick = 0;
            do
            {
                for (int y = 0; (y < height) && !crashed; y++)
                {
                    pos.y = y;
                    for (int x = 0; (x < width) && !crashed; x++)
                    {
                        pos.x = x;
                        for (int i = 0; i < carts.Count; i++)
                        {
                            if (carts[i].pos == pos)
                            {
                                if (!carts[i].moved && !carts[i].crashed)
                                {
                                    carts[i] = MoveCart(carts[i], ref cleanMap);
                                    crashed = FindNewCrashes(ref crashPos);
                                }
                            }
                        }
                    }
                }
                for (int i = 0; i < carts.Count; i++)
                {
                    Cart c = carts[i];
                    c.moved = false;
                    carts[i] = c;
                }
                //PrintMap(cleanMap, carts);
                tick++;
            }
            while (!crashed);
            Console.WriteLine("Part A: Result is " + crashPos.x + ", " + crashPos.y + " (took " + tick + " ticks).");
        }

        static void PartB()
        {
            char[,] map = ReadInput();
            //SaveMap(map, "map_b.txt");
            CreateCarts(map);
            char[,] cleanMap = CleanCarts(map);
            //SaveMap(cleanMap, "map_b_clean.txt");
            int width = map.GetLength(0);
            int height = map.GetLength(1);
            Position pos = new Position();
            Position lastPos = new Position();
            int tick = 0;
            int nCarts = 0;
            do
            {
                for (int y = 0; y < height; y++)
                {
                    pos.y = y;
                    for (int x = 0; x < width; x++)
                    {
                        pos.x = x;
                        for (int i = 0; i < carts.Count; i++)
                        {
                            if (carts[i].pos == pos)
                            {
                                if (!carts[i].moved && !carts[i].crashed)
                                {
                                    carts[i] = MoveCart(carts[i], ref cleanMap);
                                    FindNewCrashes(ref lastPos);
                                }
                            }
                        }
                    }
                }
                carts.RemoveAll(c => c.crashed);
                for (int i = 0; i < carts.Count; i++)
                {
                    Cart c = carts[i];
                    c.moved = false;
                    carts[i] = c;
                }
                nCarts = carts.Count;
                //PrintMap(cleanMap, carts);
                tick++;
            }
            while (nCarts > 1);
            lastPos = carts[0].pos;
            Console.WriteLine("Part B: Result is " + lastPos.x + ", " + lastPos.y + " (took " + tick + " ticks).");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day13).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
