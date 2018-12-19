using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;

namespace day19
{
    public struct InstructionBits
    {
        public int opcode;
        public int input1;
        public int input2;
        public int output;

        public InstructionBits(int op, int i1, int i2, int u)
        {
            opcode = op;
            input1 = i1;
            input2 = i2;
            output = u;
        }
        public int A
        {
            get { return input1; }
        }
        public int B
        {
            get { return input2; }
        }
        public int C
        {
            get { return output; }
        }
    }

    public struct Memory : IEquatable<Memory>
    {
        public int addr0;
        public int addr1;
        public int addr2;
        public int addr3;
        public int addr4;
        public int addr5;

        public Memory(int a0, int a1, int a2, int a3, int a4, int a5)
        {
            addr0 = a0;
            addr1 = a1;
            addr2 = a2;
            addr3 = a3;
            addr4 = a4;
            addr5 = a5;
        }
        public Memory(Memory m)
        {
            this = new Memory(m.addr0, m.addr1, m.addr2, m.addr3, m.addr4, m.addr5);
        }
        public int this[int key]
        {
            get
            {
                switch (key)
                {
                    case 0: return addr0;
                    case 1: return addr1;
                    case 2: return addr2;
                    case 3: return addr3;
                    case 4: return addr4;
                    case 5: return addr5;
                    default: throw new ArgumentOutOfRangeException();
                }
            }
            set
            {
                switch (key)
                {
                    case 0: addr0 = value; break;
                    case 1: addr1 = value; break;
                    case 2: addr2 = value; break;
                    case 3: addr3 = value; break;
                    case 4: addr4 = value; break;
                    case 5: addr5 = value; break;
                    default: throw new ArgumentOutOfRangeException();
                }
            }
        }
        public override bool Equals(Object obj)
        {
            return obj is Memory && Equals((Memory)obj);
        }
        public bool Equals(Memory m)
        {
            return (addr0 == m.addr0) && (addr1 == m.addr1) && (addr2 == m.addr2) && 
                (addr3 == m.addr3) && (addr4 == m.addr4) && (addr5 == m.addr5);
        }
        public override int GetHashCode()
        {
            var hashCode = 927920810;
            hashCode = hashCode * -1521134295 + base.GetHashCode();
            hashCode = hashCode * -1521134295 + addr0.GetHashCode();
            hashCode = hashCode * -1521134295 + addr1.GetHashCode();
            hashCode = hashCode * -1521134295 + addr2.GetHashCode();
            hashCode = hashCode * -1521134295 + addr3.GetHashCode();
            hashCode = hashCode * -1521134295 + addr4.GetHashCode();
            hashCode = hashCode * -1521134295 + addr5.GetHashCode();
            return hashCode;
        }
        public static bool operator ==(Memory m1, Memory m2)
        {
            return m1.Equals(m2);
        }
        public static bool operator !=(Memory m1, Memory m2)
        {
            return !m1.Equals(m2);
        }
    }

    public abstract class Instruction
    {
        private int opcode;
        public Instruction(int op)
        {
            opcode = op;
        }
        public int Opcode
        {
            get
            {
                return opcode;
            }
        }
        public abstract void Execute(InstructionBits i, ref Memory m);
    }

    // ---------------------------------------------------------------------
    public class Addr : Instruction
    {
        public Addr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] + m[i.B];
        }
    }
    public class Addi : Instruction
    {
        public Addi(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] + i.B;
        }
    }

    public class Mulr : Instruction
    {
        public Mulr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] * m[i.B];
        }
    }
    public class Muli : Instruction
    {
        public Muli(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] * i.B;
        }
    }

    public class Banr : Instruction
    {
        public Banr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] & m[i.B];
        }
    }
    public class Bani : Instruction
    {
        public Bani(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] & i.B;
        }
    }

    public class Borr : Instruction
    {
        public Borr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] | m[i.B];
        }
    }
    public class Bori : Instruction
    {
        public Bori(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A] | i.B;
        }
    }

    public class Setr : Instruction
    {
        public Setr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = m[i.A];
        }
    }
    public class Seti : Instruction
    {
        public Seti(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = i.A;
        }
    }

    public class Gtir : Instruction
    {
        public Gtir(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = (i.A > m[i.B]) ? 1 : 0;
        }
    }
    public class Gtri : Instruction
    {
        public Gtri(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = (m[i.A] > i.B) ? 1 : 0;
        }
    }
    public class Gtrr : Instruction
    {
        public Gtrr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = (m[i.A] > m[i.B]) ? 1 : 0;
        }
    }

    public class Eqir : Instruction
    {
        public Eqir(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = (i.A == m[i.B]) ? 1 : 0;
        }
    }
    public class Eqri : Instruction
    {
        public Eqri(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = (m[i.A] == i.B) ? 1 : 0;
        }
    }
    public class Eqrr : Instruction
    {
        public Eqrr(int op) : base(op) { }
        public override void Execute(InstructionBits i, ref Memory m)
        {
            m[i.C] = (m[i.A] == m[i.B]) ? 1 : 0;
        }
    }


    // ---------------------------------------------------------------------
    class Day19
    {
        static int ipRegister = -1;

        static List<InstructionBits> ReadInput()
        {
            List<InstructionBits> instructions = new List<InstructionBits>();
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line = reader.ReadLine();
            string s = new string(line.Where(Char.IsDigit).ToArray());
            ipRegister = int.Parse(s);
            Regex regex = new Regex(@"^([a-z]+) (\d+) (\d+) (\d+)");
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = regex.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    int i = 1;
                    int opcode = opcodeDictionary[groups[i++].Value];
                    InstructionBits bits = new InstructionBits(opcode, int.Parse(groups[i++].Value), int.Parse(groups[i++].Value), int.Parse(groups[i++].Value));
                    instructions.Add(bits);
                }
            }
            return instructions;
        }

        static public readonly List<Instruction> executableInstructions = new List<Instruction>
        {
            new Addr(4),
            new Addi(9),
            new Mulr(11),
            new Muli(13),
            new Banr(8),
            new Bani(15),
            new Borr(1),
            new Bori(12),
            new Setr(10),
            new Seti(5),
            new Gtir(2),
            new Gtri(7),
            new Gtrr(0),
            new Eqir(14),
            new Eqri(3),
            new Eqrr(6),
        };

        static public readonly Dictionary<string, int> opcodeDictionary =
            executableInstructions.ToDictionary(x => x.GetType().Name.ToLower(), x => x.Opcode);

        static void PartA()
        {
            Dictionary<int, Instruction> instructions = new Dictionary<int, Instruction>();
            foreach (Instruction i in executableInstructions)
            {
                instructions[i.Opcode] = i;
            }
            List<InstructionBits> testProgram = ReadInput();
            Memory memory = new Memory();
            int ip = 0;
            while ((ip >= 0) && (ip < testProgram.Count))
            {
                InstructionBits bits = testProgram[ip];
                instructions[bits.opcode].Execute(bits, ref memory);
                memory[ipRegister]++;
                ip = memory[ipRegister];
            }
            Console.WriteLine("Part A: Result is " + memory[0] + ".");
        }

        static int ExecuteDeviceProgramCalculation(int d)
        {
            int a = 0;
            for (int v = 1; v <= d; v++)
            {
                if ((d / v) * v == d)
                    a += v;
            }
            return a;
        }

        static void PartB()
        {
            int a = 0;
            int b = 0;
            int d = 0;
            //int x = 0;
            //int y = 0;

            d += 2;
            d *= d;
            d *= 19;
            d *= 11;
            b += 6;
            b *= 22;
            b += 6;
            d += b;

            a = ExecuteDeviceProgramCalculation(d);
            Console.WriteLine("Part B: Part A Verification: " + a + ".");

            b = 27;
            b *= 28;
            b += 29;
            b *= 30;
            b *= 14;
            b *= 32;
            d += b;

            //x = 1;
            //Addr1:
            //y = 1;
            //Addr2:
            //b = x * y;
            //b = (b == d) ? 1 : 0;
            //if (b == 1)
            //    a += x;
            //y += 1;
            //b = (y > d) ? 1 : 0;
            //if (b == 0)
            //    goto Addr2;
            //x += 1;
            //b = (x > d) ? 1 : 0;
            //if (b == 0)
            //    goto Addr1;

            //for (x = 1; x <= d; x++)
            //    for (y = 1; y <= d; y++)
            //        if (x * y == d)
            //            a += x;

            a = ExecuteDeviceProgramCalculation(d);
            Console.WriteLine("Part B: Result of running the program is " + a + ".");
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day19).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
