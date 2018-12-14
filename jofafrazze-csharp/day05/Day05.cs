using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day05
{
    class Day05
    {
        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day05).Namespace + ":");

            // First read input
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string input = reader.ReadLine();

            // Part A
            string ReactPolymer(string polymerIn)
            {
                string result = polymerIn;
                bool reactionTookPlace = false;
                do
                {
                    StringBuilder nextResultBuilder = new StringBuilder();
                    int i = 0;
                    int iMax = result.Length - 1;
                    for (; i < iMax; i++)
                    {
                        bool localReaction = false;
                        char a = result[i];
                        char b = result[i + 1];
                        if (Char.ToLower(a) == Char.ToLower(b))
                        {
                            if (Char.IsLower(a) != Char.IsLower(b))
                            {
                                i++;
                                localReaction = true;
                            }
                        }
                        if (!localReaction)
                        {
                            nextResultBuilder.Append(a);
                        }
                    }
                    if (i == iMax)
                    {
                        nextResultBuilder.Append(result.Last());
                    }
                    string nextResult = nextResultBuilder.ToString();
                    reactionTookPlace = (result.Length != nextResult.Length);
                    result = nextResult;
                }
                while (reactionTookPlace);
                return result;
            }
            string polymer = ReactPolymer(input);
            Console.WriteLine("Part A: Resulting number of units is " + polymer.Length + ".");

            // Part B
            int minLength = -1;
            for (char c = 'a'; c <= 'z'; c++)
            {
                string nextInput = Regex.Replace(input, c.ToString(), "", RegexOptions.IgnoreCase);
                string nextPolymer = ReactPolymer(nextInput);
                if ((nextPolymer.Length < minLength) || (minLength < 0))
                {
                    minLength = nextPolymer.Length;
                }
            }
            Console.WriteLine("Part B: Resulting minimum number of units is " + minLength + ".");
        }
    }
}
