using System;
using System.Collections.Generic;
using System.Linq;

namespace day21
{
    class Day21
    {
        // R0 => a, R1 => b, R2 => c, R3 => d, R4 => x, R5 => PC
        // 
        // #ip 5
        // seti 123 0 3            0:  R3 = 123                    d = 123     (0x7B     0111 1011)
        // bani 3 456 3            1:  R3 &= 456                   d &= 456    (0x1C8  1 1100 1000) => (7B & 1C8 = 0x48 // // (72))
        // eqri 3 72 3             2:  R3 = (R3 == 72) ?           d = (d == 72) ? 1 : 0
        // addr 3 5 5              3:  R5 += R3                    PC += d
        // seti 0 0 5              4:  R5 = 0                      PC = 0
        // seti 0 9 3              5:  R3 = 0                      d = 0
        // bori 3 65536 1          6:  R1 = R3 | 65536 (10000)     b = d | 0x10000
        // seti 14906355 8 3       7:  R3 = 14906355   (E373F3)    d = 14906355
        // bani 1 255 4            8:  R4 = R1 & 255   (FF)        x = b & FF
        // addr 3 4 3              9:  R3 += R4                    d += x
        // bani 3 16777215 3       10: R3 &= 16777215  (FFFFFF)    d &= FFFFFF
        // muli 3 65899 3          11: R3 *= 65899     (1016B)     d *= 65899
        // bani 3 16777215 3       12: R3 &= 16777215              d &= FFFFFF
        // gtir 256 1 4            13: R4 = (256 > R1) ?           x = (b < 256) ? 1 : 0
        // addr 4 5 5              14: R5 += R4                    PC += x
        // addi 5 1 5              15: R5 += 1                     PC += 1
        // seti 27 8 5             16: R5 = 27                     PC = 27
        // seti 0 4 4              17: R4 = 0                      x = 0
        // addi 4 1 2              18: R2 = R4 + 1                 c = x + 1
        // muli 2 256 2            19: R2 *= 256                   c *= 256
        // gtrr 2 1 2              20: R2 = (R2 > R1) ?            c = (c > b) ? 1 : 0
        // addr 2 5 5              21: R5 += R2                    PC += c
        // addi 5 1 5              22: R5 += 1                     PC += 1
        // seti 25 1 5             23: R5 = 25                     PC = 25
        // addi 4 1 4              24: R4 += 1                     x += 1
        // seti 17 2 5             25: R5 = 17                     PC = 17
        // setr 4 9 1              26: R1 = R4                     b = x
        // seti 7 0 5              27: R5 = 7                      PC = 7
        // eqrr 3 0 4              28: R4 = (R3 = R0) ?            x = (d = a) ? 1 : 0
        // addr 4 5 5              29: R5 += R4                    PC += x
        // seti 5 3 5              30: R5 = 5                      PC = 5

        static void PartA()
        {
            int a = 3173684;
            int b = 0;
            int c = 0;
            int d = 0;
            int x = 0;

            d = 0;

            Row5:
            b = (d | 0x10000);      // b = 0x100000, Mask d, assign to b
            d = 0xE373F3;           // d is huge

            Row7:
            x = b & 0xFF;           // x = 0
            d += x;                 // d = 0
            d &= 0xFFFFFF;          // d = 0
            d *= 0x1016B;           // d = 0
            d &= 0xFFFFFF;          // d = 0
            x = (b < 256) ? 1 : 0;  // x = 0
            if (x == 1) goto Row27;

            x = 0;                  // x = 0

            Row17:      
            c = x + 1;              // c = 1
            c *= 0x100;             // c = 0x100
            c = (c > b) ? 1 : 0;    // c = 1
            if (c == 1) goto Row25;

            x += 1;
            goto Row17;

            Row25:
            b = x;                  // b = 0
            goto Row7;

            Row27:
            x = (d == a) ? 1 : 0;   // Debugging: d is 3173684 first time run --> use a = 3173684
            if (x == 1) goto Done;
            goto Row5;

            Done:
            Console.WriteLine("Part A: Result is " + a + ".");
        }

        static List<int> CalculatePossibleDs()
        {
            HashSet<int> possibleDs = new HashSet<int>();
            int b = 0;
            int c = 0;
            int d = 0;
            int x = 0;
            uint iter = 0;
            while (true)
            {
                d = 0;

                Row5:
                b = (d | 0x10000);
                d = 0xE373F3;

                Row7:
                x = b & 0xFF;
                d += x;
                d &= 0xFFFFFF;
                d *= 0x1016B;
                d &= 0xFFFFFF;
                x = (b < 256) ? 1 : 0;
                if (x == 1) goto Row27;

                x = 0;

                Row17:
                c = x + 1;
                c *= 0x100;
                c = (c > b) ? 1 : 0;
                if (c == 1) goto Row25;

                x += 1;
                goto Row17;

                Row25:
                b = x;
                goto Row7;

                Row27:
                if (!possibleDs.Contains(d))
                {
                    //Console.WriteLine("Added #{0} = {1}", possibleDs.Count(), d);
                }
                else
                {
                    //Console.WriteLine("Duplicate! = {0}", d);
                }
                possibleDs.Add(d);
                iter++;
                if (iter == 10371)  // Running this --> after 10371 iterations the d values repeat
                {
                    break;
                }
                goto Row5;
            }
            return possibleDs.ToList();
        }

        static void PartB()
        {
            List<int> possibleDs = CalculatePossibleDs();
            Console.WriteLine("Part B: Result is " + possibleDs.Last() + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day21).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
