using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day14
{
    static class CircularLinkedList
    {
        public static LinkedListNode<T> NextOrFirst<T>(this LinkedListNode<T> current)
        {
            return current.Next ?? current.List.First;
        }

        public static LinkedListNode<T> PreviousOrLast<T>(this LinkedListNode<T> current)
        {
            return current.Previous ?? current.List.Last;
        }
    }

    class Day14
    {
        static LinkedList<int> scoreBoard = new LinkedList<int>();
        static LinkedListNode<int> pos1 = new LinkedListNode<int>(0);
        static LinkedListNode<int> pos2 = new LinkedListNode<int>(0);

        static void ReadInput()
        {
            int[] input = { 3, 7 };
            scoreBoard.Clear();
            foreach (int i in input)
            {
                scoreBoard.AddLast(i);
            }
            pos1 = scoreBoard.First;
            pos2 = pos1.Next;
        }

        static void IterateRecipe()
        {
            int sum = pos1.Value + pos2.Value;
            int a = sum / 10;
            int b = sum % 10;
            if (a > 0)
            {
                scoreBoard.AddLast(a);
            }
            scoreBoard.AddLast(b);
            int steps1 = pos1.Value + 1;
            for (int i = 0; i < steps1; i++)
            {
                pos1 = pos1.NextOrFirst();
            }
            int steps2 = pos2.Value + 1;
            for (int i = 0; i < steps2; i++)
            {
                pos2 = pos2.NextOrFirst();
            }
        }

        static string ExtractFromPos(LinkedListNode<int> pos)
        {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < 10; i++)
            {
                sb.Append(pos.Value.ToString());
                pos = pos.NextOrFirst();
            }
            return sb.ToString();
        }

        static int FindPattern(string pattern)
        {
            if (scoreBoard.Count() < pattern.Length + 2)
            {
                return -1;
            }
            LinkedListNode<int> searchPos = scoreBoard.Last;
            for (int i = 0; i < pattern.Length + 1; i++)
            {
                searchPos = searchPos.Previous;
            }
            StringBuilder sb = new StringBuilder();
            do
            {
                sb.Append(searchPos.Value.ToString());
                searchPos = searchPos.Next;
            }
            while (searchPos != null);
            string searchString = sb.ToString();
            int subPos = searchString.IndexOf(pattern);
            if (subPos >= 0)
            {
                return scoreBoard.Count() - searchString.Length + subPos;
            }
            return -1;
        }

        static void PartA()
        {
            ReadInput();
            const int nPreRecipes = 864801;
            while(scoreBoard.Count() < nPreRecipes + 10)
            {
                IterateRecipe();
            }
            LinkedListNode<int> pos = scoreBoard.Last;
            for (int i = scoreBoard.Count() - 1; i > nPreRecipes; i--)
            {
                pos = pos.Previous;
            }
            //Console.WriteLine("{0}", string.Join("", scoreBoard));
            string s = ExtractFromPos(pos);
            Console.WriteLine("Part A: Result after " + nPreRecipes + " is " + s + ".");
        }

        static void PartB()
        {
            ReadInput();
            string pattern = "864801";
            int pos = -1;
            while (pos < 0)
            {
                IterateRecipe();
                pos = FindPattern(pattern);
            }
            Console.WriteLine("Part B: Result is " + pos + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day14).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
