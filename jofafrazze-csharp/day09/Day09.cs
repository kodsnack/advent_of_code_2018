using System;
using System.Collections.Generic;
using System.Linq;

namespace day09
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

    class Day09
    {
        static long PlayGame(int nPlayers, int lastMarble)
        {
            long[] score = new long[nPlayers];
            LinkedList<int> circle = new LinkedList<int>();
            LinkedListNode<int> cPos = circle.AddFirst(0);
            int marble = 1;
            int player = 0;
            for (int i = 1; i <= lastMarble; i++)
            {
                if (marble % 23 == 0)
                {
                    score[player] += marble;
                    cPos = cPos.PreviousOrLast().PreviousOrLast().PreviousOrLast();
                    cPos = cPos.PreviousOrLast().PreviousOrLast().PreviousOrLast();
                    var scorePos = cPos;
                    cPos = cPos.PreviousOrLast();
                    circle.Remove(scorePos);
                    score[player] += scorePos.Value;
                }
                else
                {
                    cPos = cPos.NextOrFirst().NextOrFirst();
                    circle.AddAfter(cPos, marble);
                }
                marble++;
                player = (player + 1) % nPlayers;
            }
            return score.Max();
        }

        static void PartA()
        {
            long score = PlayGame(419, 71052);
            Console.WriteLine("Part A: Result is " + score + ".");
        }

        static void PartB()
        {
            long score = PlayGame(419, 71052 * 100);
            Console.WriteLine("Part B: Result is " + score + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day09).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
