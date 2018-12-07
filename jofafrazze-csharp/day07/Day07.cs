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

    public class Node
    {
        public char id;
        public List<char> prereq;

        public Node()
        {
            id = '0';
            prereq = new List<char>();
        }

        public Node DeepCopy()
        {
            Node copy = (Node)this.MemberwiseClone();
            copy.prereq = new List<char>(prereq);
            return copy;
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
            Regex parts = new Regex(@"^Step (\w) must be finished before step (\w)");
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
            Dictionary<char, Node> nodes = new Dictionary<char, Node>();
            foreach(Step s in input)
            {
                Node a = new Node();
                a.id = s.prereqChar;
                if (!nodes.ContainsKey(a.id))
                {
                    nodes[a.id] = a;
                }
                Node b = new Node();
                b.id = s.followChar;
                if (!nodes.ContainsKey(b.id))
                {
                    nodes[b.id] = b;
                }
            }
            foreach (Step s in input)
            {
                nodes[s.followChar].prereq.Add(s.prereqChar);
            }
            Dictionary<char, Node> nodesCopy = new Dictionary<char, Node>();
            foreach (KeyValuePair<char, Node> kvp in nodes)
            {
                nodesCopy[kvp.Key] = kvp.Value.DeepCopy();
            }
            List<char> result = new List<char>();
            while (nodes.Any())
            {
                List<char> candidates = new List<char>();
                foreach (KeyValuePair<char, Node> kvp in nodes)
                {
                    if (!kvp.Value.prereq.Any())
                    {
                        candidates.Add(kvp.Value.id);
                    }
                }
                candidates.Sort();
                if (candidates.Any())
                {
                    char c = candidates.First();
                    result.Add(c);
                    foreach (KeyValuePair<char, Node> kvp in nodes)
                    {
                        kvp.Value.prereq.Remove(c);
                    }
                    nodes.Remove(c);
                }
            }
            Console.WriteLine("Part A: Take steps in this order: " + String.Join("", result) + ".");

            // Part B
            string resultB = "";
            int totalSecs = 0;
            int stepSecs = 0;
            nodes = nodesCopy;
            int nWorkers = 5;
            int offsSecs = 60;
            List<Worker> workers = new List<Worker>();
            SortedDictionary<int, char> workerDoneTime = new SortedDictionary<int, char>();
            while (nodes.Any())
            {
                List<char> candidates = new List<char>();
                foreach (KeyValuePair<char, Node> kvp in nodes)
                {
                    if (!kvp.Value.prereq.Any())
                    {
                        candidates.Add(kvp.Value.id);
                    }
                }
                stepSecs = 0;
                candidates.Sort();
                while (candidates.Any() && (workers.Count() < nWorkers))
                {
                    char c = candidates.First();
                    candidates.RemoveAt(0);
                    Worker w = new Worker();
                    w.id = c;
                    w.timeDone = totalSecs + offsSecs + (c - 'A' + 1);
                    workers.Add(w);
                    nodes.Remove(c);
                    resultB += c;
                }
                int tNext = int.MaxValue;
                foreach (Worker w in workers)
                {
                    if (w.timeDone < tNext)
                    {
                        tNext = w.timeDone;
                    }
                }
                foreach (Worker w in workers)
                {
                    if (w.timeDone == tNext)
                    {
                        foreach (KeyValuePair<char, Node> kvp in nodes)
                        {
                            kvp.Value.prereq.Remove(w.id);
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
