using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;

namespace day10
{
    public struct Point
    {
        public int x;
        public int y;
    }
    public struct Vector
    {
        public Point pos;
        public Point speed;
    }

    class Day10
    {
        static Vector[] ReadInput()
        {
            List<Vector> vectors = new List<Vector>();
            Regex parts = new Regex(@"^.*<([ -]\d+),\s+(-?\d+).*<([ -]\d+),\s+(-?\d+)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    Vector v = new Vector();
                    int i = 1;
                    v.pos.x = int.Parse(groups[i++].Value);
                    v.pos.y = int.Parse(groups[i++].Value);
                    v.speed.x = int.Parse(groups[i++].Value);
                    v.speed.y = int.Parse(groups[i++].Value);
                    vectors.Add(v);
                }
            }
            return vectors.ToArray();
        }

        static void StepMessage(List<Vector> vectors)
        {
            for (int i = 0; i < vectors.Count; i++)
            {
                Vector v = vectors[i];
                v.pos.x += v.speed.x;
                v.pos.y += v.speed.y;
                vectors[i] = v;
            }
        }

        static int Span(List<int> l)
        {
            return l.Max() - l.Min() + 1;
        }

        static int CalcSize(List<Point> message)
        {
            int w = Span(message.Select(a => a.x).ToList());
            int h = Span(message.Select(a => a.y).ToList());
            return w + h;
        }

        static void PrintOut(List<Point> message)
        {
            int xMin = message.Min(a => a.x);
            int yMin = message.Min(a => a.y);
            int xSteps = Span(message.Select(a => a.x).ToList());
            int ySteps = Span(message.Select(a => a.y).ToList());
            int[,] matrix = new int[xSteps, ySteps];
            foreach (Point p in message)
            {
                matrix[p.x - xMin, p.y - yMin] = 1;
            }
            for (int y = 0; y < ySteps; y++)
            {
                StringBuilder sb = new StringBuilder();
                for (int x = 0; x < xSteps; x++)
                {
                    sb.Append(matrix[x, y] == 0 ? " " : "#");
                }
                Console.WriteLine(sb.ToString());
            }
        }

        static void PartAB()
        {
            List<Vector> vectors = new List<Vector>(ReadInput());
            int size = int.MaxValue;
            int iterations = 0;
            bool done = false;
            do
            {
                StepMessage(vectors);
                int s = CalcSize(vectors.Select(a => a.pos).ToList());
                if (s < size)
                {
                    size = s;
                }
                else
                {
                    done = true;
                }
                iterations++;
            }
            while (!done);
            vectors = new List<Vector>(ReadInput());
            for (int i = 0; i < iterations - 1; i++)
            {
                StepMessage(vectors);
            }
            Console.WriteLine("Part A: Result is:");
            PrintOut(vectors.Select(a => a.pos).ToList());
            Console.WriteLine("Part B: Result is " + (iterations - 1) + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day10).Namespace + ":");
            PartAB();
        }
    }
}
