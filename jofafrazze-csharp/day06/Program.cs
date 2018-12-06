using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day06
{
    class Program
    {
        public struct Coord
        {
            public int id;
            public int x;
            public int y;
        };

        public struct Area
        {
            public int area;
            public bool atEdge;
        };

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Program).Namespace + ":");

            // First read input
            List<Coord> coords = new List<Coord>();
            Regex parts = new Regex(@"^(\d+), (\d+)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            int coordId = 0;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    Coord coord = new Coord();
                    int i = 1;
                    coord.id = coordId++;
                    coord.x = int.Parse(groups[i++].Value);
                    coord.y = int.Parse(groups[i++].Value);
                    coords.Add(coord);
                }
            }
            if (coords.Count != 50)
            {
                return;
            }

            // Part A
            int[] xCoords = new int[coords.Count];
            int[] yCoords = new int[coords.Count];
            foreach (Coord c in coords)
            {
                xCoords[c.id] = c.x;
                yCoords[c.id] = c.y;
            }
            int xMin = xCoords.Min() - 1;
            int xMax = xCoords.Max() + 1;
            int yMin = yCoords.Min() - 1;
            int yMax = yCoords.Max() + 1;
            int mapWidth = xMax - xMin + 1;
            int mapHeight = yMax - yMin + 1;
            int[,] distanceMap = new int[mapWidth, mapHeight];
            // Find min dist in all map coordinates
            for (int x = xMin; x <= xMax; x++)
            {
                for (int y = yMin; y <= yMax; y++)
                {
                    int minDist = int.MaxValue;
                    int minDistId = 0;
                    foreach (Coord c in coords)
                    {
                        int dist = Math.Abs(x - c.x) + Math.Abs(y - c.y);
                        if (dist < minDist)
                        {
                            minDist = dist;
                            minDistId = c.id;
                        }
                        else if (dist == minDist)
                        {
                            minDistId = -1;
                        }
                    }
                    distanceMap[x - xMin, y - yMin] = minDistId;
                }
            }
            // Sum up all areas
            Area[] areas = new Area[coords.Count];
            for (int x = xMin; x <= xMax; x++)
            {
                for (int y = yMin; y <= yMax; y++)
                {
                    int closest = distanceMap[x - xMin, y - yMin];
                    if (closest >= 0)
                    {
                        areas[closest].area++;
                    }
                }
            }
            // Find areas touching the outer edges
            for (int x = 0; x < mapWidth; x++)
            {
                int e1 = distanceMap[x, 0];
                if (e1 >= 0)
                {
                    areas[e1].atEdge = true;
                }
                int e2 = distanceMap[x, mapHeight - 1];
                if (e2 >= 0)
                {
                    areas[e2].atEdge = true;
                }
            }
            for (int y = 0; y < mapHeight; y++)
            {
                int e1 = distanceMap[0, y];
                if (e1 >= 0)
                {
                    areas[e1].atEdge = true;
                }
                int e2 = distanceMap[mapWidth - 1, y];
                if (e2 >= 0)
                {
                    areas[e2].atEdge = true;
                }
            }
            int maxFiniteArea = 0;
            foreach(Area a in areas)
            {
                if ((a.area > maxFiniteArea) && !a.atEdge)
                {
                    maxFiniteArea = a.area;
                }
            }
            Console.WriteLine("Part A: Largest finite area is " + maxFiniteArea + ".");

            // Part B
            int[,] totDistMap = new int[mapWidth, mapHeight];
            int nCloseEnough = 0;
            for (int x = xMin; x <= xMax; x++)
            {
                for (int y = yMin; y <= yMax; y++)
                {
                    foreach (Coord c in coords)
                    {
                        int dist = Math.Abs(x - c.x) + Math.Abs(y - c.y);
                        totDistMap[x - xMin, y - yMin] += dist;
                    }
                    if (totDistMap[x - xMin, y - yMin] < 10000)
                    {
                        nCloseEnough++;
                    }
                }
            }
            Console.WriteLine("Part B: Area size is " + nCloseEnough + ".");
        }
    }
}
