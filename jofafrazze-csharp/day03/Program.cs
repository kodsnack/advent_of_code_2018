using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day03
{
    class Program
    {
        public struct Claim
        {
            public int id;
            public int left;
            public int top;
            public int width;
            public int height;
        };

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Program).Namespace + ":");

            // First read input
            List<Claim> claims = new List<Claim>();
            Regex parts = new Regex(@"^\#(\d+) @ (\d+),(\d+)\: (\d+)x(\d+)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    Claim claim = new Claim();
                    claim.id = int.Parse(groups[1].Value);
                    claim.left = int.Parse(groups[2].Value);
                    claim.top = int.Parse(groups[3].Value);
                    claim.width = int.Parse(groups[4].Value);
                    claim.height = int.Parse(groups[5].Value);
                    claims.Add(claim);
                }
            }
            if (claims.Count != 1335)
            {
                return;
            }

            // Part A
            int[,] fabric = new int[1000, 1000];
            foreach (Claim claim in claims)
            {
                for (int x = 0; x < claim.width; x++)
                {
                    for (int y = 0; y < claim.height; y++)
                    {
                        fabric[claim.left + x, claim.top + y] += 1;
                    }
                }
            }
            int sum = 0;
            for (int x = 0; x < 1000; x++)
            {
                for (int y = 0; y < 1000; y++)
                {
                    if (fabric[x, y] > 1)
                    {
                        sum++;
                    }
                }
            }
            Console.WriteLine("Part A: Number of overlapping fabric inches are " + sum + ".");

            // Part B
            for (int i = 0; i < claims.Count; i++)
            {
                Claim claim = claims[i];
                bool alone = true;
                for (int x = 0; (x < claim.width) && alone; x++)
                {
                    for (int y = 0; (y < claim.height) && alone; y++)
                    {
                        if (fabric[claim.left + x, claim.top + y] != 1)
                        {
                            alone = false;
                        }
                    }
                }
                if (alone)
                {
                    Console.WriteLine("Part B: Claim with id " + claim.id + " is not overlapping.");
                    return;
                }
            }
        }
    }
}
