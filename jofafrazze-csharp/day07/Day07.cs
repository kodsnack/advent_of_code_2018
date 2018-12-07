using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day07
{
    public struct Step
    {
        public char prereqChar;
        public char followChar;
    }

    public struct Node
    {
        public char id;
        public List<char> prereq;

        public Node(char c)
        {
            id = c;
            prereq = new List<char>();
        }

        public Node DeepCopy()
        {
            Node n = new Node(id);
            n.prereq = new List<char>(prereq);
            return n;
        }
    }

    public struct Worker
    {
        public char id;
        public int timeDone;
    }

    class Day07
    {
        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day07).Namespace + ":");

            // First read input
            List<Step> input = new List<Step>();
            Regex parts = new Regex(@"^S.*([A-Z]).*([A-Z])");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    Step step = new Step();
                    int i = 1;
                    step.prereqChar = groups[i++].Value[0];
                    step.followChar = groups[i++].Value[0];
                    input.Add(step);
                }
            }

            // Part A
            Dictionary<char, Node> nodeDict = new Dictionary<char, Node>();
            foreach(Step s in input)
            {
                nodeDict[s.prereqChar] = new Node(s.prereqChar);
                nodeDict[s.followChar] = new Node(s.followChar);
            }
            foreach (Step s in input)
            {
                nodeDict[s.followChar].prereq.Add(s.prereqChar);
            }
            List<Node> nodes = nodeDict.Values.ToList();
            List<Node> nodesCopy = new List<Node>();
            foreach (Node n in nodes)
            {
                nodesCopy.Add(n.DeepCopy());
            }
            List<char> result = new List<char>();
            while (nodes.Any())
            {
                List<char> candidates = new List<char>();
                foreach (Node n in nodes)
                {
                    if (!n.prereq.Any())
                    {
                        candidates.Add(n.id);
                    }
                }
                candidates.Sort();
                if (candidates.Any())
                {
                    char c = candidates.First();
                    result.Add(c);
                    foreach (Node n in nodes)
                    {
                        n.prereq.Remove(c);
                    }
                    nodes.RemoveAll(n => n.id == c);
                }
            }
            string resultString = new string(result.ToArray());
            Console.WriteLine("Part A: Take steps in this order: " + resultString + ".");

            // Part B
            int totalSecs = 0;
            nodes = nodesCopy;
            const int nWorkers = 5;
            const int offsSecs = 60;
            List<Worker> workers = new List<Worker>();
            while (nodes.Any())
            {
                List<char> candidates = new List<char>();
                foreach (Node n in nodes)
                {
                    if (!n.prereq.Any())
                    {
                        candidates.Add(n.id);
                    }
                }
                candidates.Sort();
                while (candidates.Any() && (workers.Count() < nWorkers))
                {
                    char c = candidates.First();
                    candidates.RemoveAt(0);
                    Worker w = new Worker
                    {
                        id = c,
                        timeDone = totalSecs + offsSecs + (c - 'A' + 1)
                    };
                    workers.Add(w);
                    nodes.RemoveAll(n => n.id == c);
                }
                int tNext = workers.Min(w => w.timeDone);
                foreach (Worker w in workers)
                {
                    if (w.timeDone == tNext)
                    {
                        foreach (Node n in nodes)
                        {
                            n.prereq.Remove(w.id);
                        }
                    }
                }
                workers.RemoveAll(t => t.timeDone == tNext);
                totalSecs = tNext;
            }
            Console.WriteLine("Part B: All steps complete in " + totalSecs + " seconds.");
        }
    }
}
