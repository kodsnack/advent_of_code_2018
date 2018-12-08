using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day08
{
    public class Node
    {
        public List<Node> children;
        public List<int> metadata;
        public long sum;
        public Node()
        {
            children = new List<Node>();
            metadata = new List<int>();
            sum = 0;
        }
    }

    class Day08
    {
        static int[] ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string input = reader.ReadLine();
            int[] data = input.Split(' ').Select(int.Parse).ToArray();
            //int[] data = { 2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2 };
            return data;
        }

        static void PartA()
        {
            int[] data = ReadInput();
            Queue<int> input = new Queue<int>(data);
            List<Node> nodes = new List<Node>();
            void ReadNode(Queue<int> q)
            {
                int nChild = q.Dequeue();
                int nMeta = q.Dequeue();
                Node node = new Node();
                for (int c = 0; c < nChild; c++)
                {
                    ReadNode(q);
                }
                for (int n = 0; n < nMeta; n++)
                {
                    node.metadata.Add(q.Dequeue());
                }
                nodes.Add(node);
            }
            ReadNode(input);
            int sumMeta = nodes.Sum(m => m.metadata.Sum());
            Console.WriteLine("Part A: Result is " + sumMeta + ".");
        }

        static void PartB()
        {
            int[] data = ReadInput();
            Queue<int> input = new Queue<int>(data);
            Node ReadNode(Queue<int> q)
            {
                int nChildren = q.Dequeue();
                int nMeta = q.Dequeue();
                Node node = new Node();
                for (int c = 0; c < nChildren; c++)
                {
                    node.children.Add(ReadNode(q));
                }
                for (int n = 0; n < nMeta; n++)
                {
                    node.metadata.Add(q.Dequeue());
                }
                if (nChildren > 0)
                {
                    foreach (int i in node.metadata)
                    {
                        if ((i > 0) && (i <= nChildren))
                        {
                            node.sum += node.children[i - 1].sum;
                        }
                    }
                }
                else
                {
                    node.sum = node.metadata.Sum();
                }
                return node;
            }
            Node head = ReadNode(input);
            Console.WriteLine("Part B: Result is " + head.sum + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day08).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
