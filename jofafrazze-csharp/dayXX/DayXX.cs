using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace dayXX
{
    class DayXX
    {
        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(DayXX).Namespace + ":");

            // First read input
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            //StreamReader reader = File.OpenText(path);
            //string input = reader.ReadLine();

            // Part A
            Console.WriteLine("Part A: Result is " + 'A' + ".");

            // Part B
            //Console.WriteLine("Part B: Result is " + 'B' + ".");
        }
    }
}
