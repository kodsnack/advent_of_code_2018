using System;
using System.IO;
using System.Reflection;

namespace day11
{
    class Day11
    {
        static int CellValue(int x, int y, int serial)
        {
            int r = x + 10;
            int p = y * r;
            p += serial;
            p *= r;
            int h = (int) Math.Floor((p % 1000) / 100.0);
            return h - 5;
        }

        static int SumCells(ref int[,] cells, int cx, int cy, int size)
        {
            int sum = 0;
            for (int a = 0; a < size; a++)
            {
                for (int b = 0; b < size; b++)
                {
                    sum += cells[cx + a, cy + b];
                }
            }
            return sum;
        }

        public static readonly int serial = 4151;
        public static readonly int fSize = 300;
        public static int[,] fuelCells = new int[fSize, fSize];

        static void PartA()
        {
            for (int x = 0; x < fSize; x++)
            {
                for (int y = 0; y < fSize; y++)
                {
                    fuelCells[x, y] = CellValue(x + 1, y + 1, serial);
                }
            }
            int xMax = 0;
            int yMax = 0;
            int sumMax = 0;
            const int wSize = 3;
            int xyMax = fSize - wSize + 1;
            for (int x = 0; x < xyMax; x++)
            {
                for (int y = 0; y < xyMax; y++)
                {
                    int sum = SumCells(ref fuelCells, x, y, wSize);
                    if (sum > sumMax)
                    {
                        sumMax = sum;
                        xMax = x;
                        yMax = y;
                    }
                }
            }
            Console.WriteLine("Part A: Result is " + (xMax + 1) + "," + (yMax + 1) + ".");
        }

        static int SumCellsZoom(ref int[,,] cells, int x, int y, int size, int origSize, int depth)
        {
            int sum = 0;
            if (size == 1)
            {
                for (int b = y; b < y + origSize; b++)
                {
                    int c = cells[depth, x, b];
                    sum += c;
                }
            }
            else
            {
                int xStart1 = x;
                int xEnd1 = ((x + 1) / 2) * 2;
                int xEnd2 = x + size;
                int xStart2 = xEnd2 - (xEnd2 % 2);
                for (int a = xStart1; a < xEnd1; a++)
                {
                    for (int b = y; b < y + origSize; b++)
                    {
                        sum += cells[depth, a, b];
                    }
                }
                if (xEnd1 != xStart2)
                {
                    sum += SumCellsZoom(ref cells, xEnd1 / 2, y, (xStart2 - xEnd1) / 2, origSize, depth - 1);
                }
                for (int a = xStart2; a < xEnd2; a++)
                {
                    for (int b = y; b < y + origSize; b++)
                    {
                        sum += cells[depth, a, b];
                    }
                }
            }
            return sum;
        }

        static void PartB()
        {
            int depth = (int) Math.Ceiling(Math.Log(fSize, 2));
            int gSize = (int) Math.Pow(2, depth);
            int[,,] cells = new int[depth + 1, gSize, gSize];
            for (int x = 0; x < fSize; x++)
            {
                for (int y = 0; y < fSize; y++)
                {
                    cells[depth, x, y] = CellValue(x + 1, y + 1, serial);
                }
            }
            int size = gSize / 2;
            for (int d = depth - 1; d >= 0; d--)
            {
                for (int x = 0; x < size; x++)
                {
                    for (int y = 0; y < fSize; y++)
                    {
                        int sum = 0;
                        for (int a = 0; a < 2; a++)
                        {
                            sum += cells[d + 1, x * 2 + a, y];
                        }
                        cells[d, x, y] = sum;
                    }
                }
                size /= 2;
            }
            int xMax = 0;
            int yMax = 0;
            int sMax = 0;
            int sumMax = 0;
            const int sStart = 1;
            int sStop = fSize;
            for (int s = sStart; s <= sStop; s++)
            {
                int xyMax = fSize - s + 1;
                for (int x = 0; x < xyMax; x++)
                {
                    for (int y = 0; y < xyMax; y++)
                    {
                        int sum = SumCellsZoom(ref cells, x, y, s, s, depth);
                        //int sum = SumCells(ref fuelCells, x, y, s);
                        if (sum > sumMax)
                        {
                            sumMax = sum;
                            xMax = x;
                            yMax = y;
                            sMax = s;
                        }
                    }
                }
                Console.Write("Part B: Processing " + s + "/" + sStop + "\r");
            }
            Console.WriteLine("Part B: Result (using serial #" + serial + ") is " + 
                (xMax + 1) + "," + (yMax + 1) + " and size is " + sMax + 
                " (power = " + sumMax + ").");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day11).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
