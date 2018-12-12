using System;
using System.Collections.Generic;
using System.IO;
using System.Reflection;
using System.Text;
using System.Threading;

namespace day12
{
    public struct Plant
    {
        public long id;
        public bool alive;
        public bool nextAlive;
    }

    public struct Rule
    {
        public bool pbb;
        public bool pb;
        public bool p;
        public bool pa;
        public bool paa;
        public bool pNext;
    }
    class Day12
    {
        static Tuple<List<Plant>, List<Rule>> ReadInput()
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line = reader.ReadLine();
            const int iOffs = 15;
            List<Plant> plants = new List<Plant>();
            for (int i = iOffs; i < line.Length; i++)
            {
                Plant p = new Plant();
                p.id = i - iOffs;
                p.alive = (line[i] == '#');
                plants.Add(p);
            }
            reader.ReadLine();
            List<Rule> rules = new List<Rule>();
            while ((line = reader.ReadLine()) != null)
            {
                int i = 0;
                Rule r = new Rule();
                r.pbb = line[i++] == '#';
                r.pb = line[i++] == '#';
                r.p = line[i++] == '#';
                r.pa = line[i++] == '#';
                r.paa = line[i++] == '#';
                i += 4;
                r.pNext = line[i++] == '#';
                rules.Add(r);
            }
            return new Tuple<List<Plant>, List<Rule>>(plants, rules);
        }

        static void PrintPlants(IEnumerable<Plant> plants)
        {
            StringBuilder sb = new StringBuilder();
            foreach (Plant p in plants)
            {
                sb.Append(p.alive ? '#' : '.');
            }
            Console.WriteLine(sb.ToString());
        }

        static void PadPlants(List<Plant> plants)
        {
            while (plants[0].alive || plants[1].alive || plants[2].alive || plants[3].alive || plants[4].alive)
            {
                Plant pb = new Plant();
                pb.id = plants[0].id - 1;
                plants.Insert(0, pb);
            }
            int m = plants.Count - 1;
            while (plants[m].alive || plants[m - 1].alive || plants[m - 2].alive || plants[m - 3].alive || plants[m - 4].alive)
            {
                Plant pa = new Plant();
                pa.id = plants[m].id + 1;
                plants.Add(pa);
                m = plants.Count - 1;
            }
        }
        static void PadPlants(ref LinkedList<Plant> plants)
        {
            bool anyAlive = false;
            LinkedListNode<Plant> plant = plants.First;
            do
            {
                anyAlive = false;
                for (int i = 0; i < 5; i++)
                {
                    anyAlive |= plant.Value.alive;
                    plant = plant.Next;
                }
                if (anyAlive)
                {
                    Plant p = new Plant();
                    p.id = plants.First.Value.id - 1;
                    plants.AddFirst(p);
                    plant = plants.First;
                }
            }
            while (anyAlive);
            plant = plants.Last;
            do
            {
                anyAlive = false;
                for (int i = 0; i < 5; i++)
                {
                    anyAlive |= plant.Value.alive;
                    plant = plant.Previous;
                }
                if (anyAlive)
                {
                    Plant p = new Plant();
                    p.id = plants.Last.Value.id + 1;
                    plants.AddLast(p);
                    plant = plants.Last;
                }
            }
            while (anyAlive);
        }

        static void PartA()
        {
            Tuple<List<Plant>, List<Rule>> t = ReadInput();
            List<Plant> plants = t.Item1;
            List<Rule> rules = t.Item2;
            //PrintPlants(plants);
            for (int iter = 0; iter < 20; iter++)
            {
                PadPlants(plants);
                List<Plant> nextGen = new List<Plant>(plants);
                for (int i = 0; i < nextGen.Count; i++)
                {
                    Plant p = nextGen[i];
                    p.alive = false;
                    nextGen[i] = p;
                }
                foreach (Rule r in rules)
                {
                    int iLast = plants.Count - 3;
                    for (int i = 2; i <= iLast; i++)
                    {
                        bool b1 = r.pbb == plants[i - 2].alive;
                        bool b2 = r.pb == plants[i - 1].alive;
                        bool b3 = r.p == plants[i - 0].alive;
                        bool b4 = r.pa == plants[i + 1].alive;
                        bool b5 = r.paa == plants[i + 2].alive;
                        if (b1 && b2 && b3 && b4 && b5)
                        {
                            Plant p = nextGen[i];
                            p.alive = r.pNext;
                            nextGen[i] = p;
                        }
                    }
                }
                plants = nextGen;
                //PrintPlants(plants);
            }
            long sum = 0;
            foreach (Plant p in plants)
            {
                if (p.alive)
                {
                    sum += p.id;
                }
            }
            Console.WriteLine("Part A: Result is " + sum + ".");
        }

        static void PartB()
        {
            Tuple<List<Plant>, List<Rule>> t = ReadInput();
            LinkedList<Plant> plants = new LinkedList<Plant>(t.Item1);
            List<Rule> rules = t.Item2;
            const long iterMax = 50000000000;
            bool plantsChanged = true;
            //PrintPlants(plants);
            long iter = 0;
            for (; (iter < iterMax) && plantsChanged; iter++)
            {
                plantsChanged = true;
                PadPlants(ref plants);
                LinkedListNode<Plant> plant1 = plants.First;
                LinkedListNode<Plant> plant2 = plant1.Next;
                LinkedListNode<Plant> plant3 = plant2.Next;
                LinkedListNode<Plant> plant4 = plant3.Next;
                LinkedListNode<Plant> plant5 = plant4.Next;
                do
                {
                    foreach (Rule r in rules)
                    {
                        bool b1 = r.pbb == plant1.Value.alive;
                        bool b2 = r.pb == plant2.Value.alive;
                        bool b3 = r.p == plant3.Value.alive;
                        bool b4 = r.pa == plant4.Value.alive;
                        bool b5 = r.paa == plant5.Value.alive;
                        if (b1 && b2 && b3 && b4 && b5)
                        {
                            Plant p = plant3.Value;
                            p.nextAlive = r.pNext;
                            plant3.Value = p;
                        }
                    }
                    plant1 = plant2;
                    plant2 = plant3;
                    plant3 = plant4;
                    plant4 = plant5;
                    plant5 = plant5.Next;
                }
                while (plant5 != null);
                LinkedListNode<Plant> plant = plants.First;
                bool allAlikeTheNext = true;
                do
                {
                    Plant p = plant.Value;
                    if (plant.Next != null)
                    {
                        if (p.alive != plant.Next.Value.nextAlive)
                        {
                            allAlikeTheNext = false;
                        }
                    }
                    p.alive = p.nextAlive;
                    p.nextAlive = false;
                    plant.Value = p;
                    plant = plant.Next;
                }
                while (plant != null);
                plantsChanged = !allAlikeTheNext;
                //PrintPlants(plants);
                //Thread.Sleep(100);
            }
            long plantAdd = iterMax - iter;
            long sum = 0;
            foreach (Plant p in plants)
            {
                if (p.alive)
                {
                    sum += (p.id + plantAdd);
                }
            }
            Console.WriteLine("Part B: Result is " + sum + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day12).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
