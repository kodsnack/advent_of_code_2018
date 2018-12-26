using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day24
{
    public enum Distinction
    {
        Fire,
        Cold,
        Radiation,
        Bludgeoning,
        Slashing
    };

    public class Group
    {
        public int id;
        public bool isImmuneSystem;
        public int units;
        public int hitPoints;
        public int attackDamage;
        public Distinction attackType;
        public int initiative;
        public List<Distinction> immuneTo;
        public List<Distinction> weakTo;
        public int EffectivePower { get { return units * attackDamage; } }
        public Group selectedTarget;
    }

    class Day24
    {
        static void SaveTeam(List<Group> team, string header, bool append)
        {
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\" + "teams.txt");
            StreamWriter writer = new StreamWriter(path, append);
            writer.WriteLine(header);
            foreach (Group g in team)
            {
                writer.Write("{0} units each with {1} hit points ", g.units, g.hitPoints);
                if (g.immuneTo.Count > 0 || g.weakTo.Count > 0)
                {
                    char[] trimChars = new char[] { ' ', ',' };
                    string str = "(";
                    if (g.immuneTo.Count > 0)
                    {
                        str += "immune to ";
                        foreach (Distinction d in g.immuneTo)
                        {
                            str += (d.ToString().ToLower() + ", ");
                        }
                        str = str.TrimEnd(trimChars) + "; ";
                    }
                    if (g.weakTo.Count > 0)
                    {
                        str += "weak to ";
                        foreach (Distinction d in g.weakTo)
                        {
                            str += (d.ToString().ToLower() + ", ");
                        }
                        str = str.TrimEnd(trimChars);
                    }
                    writer.Write(str + ") ");
                }
                writer.WriteLine(" with an attack that does {0} {1} damage at initiative {2}", 
                    g.attackDamage, g.attackType.ToString().ToLower(), g.initiative);
            }
            writer.WriteLine();
            writer.Close();
        }

        static List<Group> ReadInput(string whichSystem, bool isImmuneSystem)
        {
            List<Distinction> GetDistinctions(string s, string text)
            {
                List<Distinction> list = new List<Distinction>();
                Regex dParts = new Regex(".*?" + s + @" ([a-z ,]+)[;\)]");
                MatchCollection dMatches = dParts.Matches(text);
                if (dMatches.Count > 0)
                {
                    GroupCollection groups = dMatches[0].Groups;
                    int i = 1;
                    string[] v = groups[i++].Value.Split(',');
                    foreach (string a in v)
                    {
                        string str = a.Trim();
                        str = char.ToUpper(str[0]) + str.Substring(1);
                        if (Enum.TryParse(str, out Distinction distinction))
                        {
                            list.Add(distinction);
                        }
                        else
                        {
                            throw new ArgumentOutOfRangeException();
                        }
                    }
                }
                return list;
            }
            List<Group> system = new List<Group>();
            Regex parts = new Regex(@"^(\d+).*?(\d+) hit points(.*?)with.*?(\d+) ([a-z]+).*?(\d+)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            bool readLines = false;
            bool readLinesNext = false;
            int id = 1;
            while ((line = reader.ReadLine()) != null)
            {
                if (line == whichSystem)
                {
                    readLinesNext = true;
                }
                if (readLines)
                {
                    MatchCollection matches = parts.Matches(line);
                    if (matches.Count > 0)
                    {
                        GroupCollection groups = matches[0].Groups;
                        Group g = new Group();
                        g.id = id++;
                        g.isImmuneSystem = isImmuneSystem;
                        int i = 1;
                        g.units = int.Parse(groups[i++].Value);
                        g.hitPoints = int.Parse(groups[i++].Value);
                        string distinctions = groups[i++].Value;
                        g.immuneTo = GetDistinctions("immune to", distinctions);
                        g.weakTo = GetDistinctions("weak to", distinctions);
                        g.attackDamage = int.Parse(groups[i++].Value);
                        string ats = groups[i++].Value;
                        ats = char.ToUpper(ats[0]) + ats.Substring(1);
                        if (!Enum.TryParse(ats, out g.attackType))
                        {
                            throw new ArgumentOutOfRangeException();
                        }
                        g.initiative = int.Parse(groups[i++].Value);
                        system.Add(g);
                    }
                    else
                    {
                        readLinesNext = false;
                    }
                }
                readLines = readLinesNext;
            }
            return system;
        }

        static void PerformAttack(Group group, Group other, bool verbose)
        {
            int i = other.immuneTo.Contains(group.attackType) ? 0 : 1;
            int w = other.weakTo.Contains(group.attackType) ? 2 : 1;
            int damage = group.EffectivePower * i * w;
            int unitsDown = Math.Min(damage / other.hitPoints, other.units);
            other.units -= unitsDown;
            if (verbose)
            {
                string head = group.isImmuneSystem ? "Immune System" : "Infection";
                Console.WriteLine("{0} group {1} attacks defending group {2}, killing {3} units",
                    head, group.id, other.id, unitsDown);
            }
        }

        static Group SelectTarget(Group group, List<Group> allTeams, bool verbose)
        {
            List<Group> otherTeam = GetLiveGroups(!group.isImmuneSystem, allTeams);
            Group toAttack = null;
            int damageDelt = -1;
            int defendingEffectivePower = 0;
            int defendersInitiative = 0;
            void ChoseTarget(Group g, int d)
            {
                toAttack = g;
                damageDelt = d;
                defendingEffectivePower = g.EffectivePower;
                defendersInitiative = g.initiative;
                if (verbose)
                {
                    string head = group.isImmuneSystem ? "Immune System" : "Infection";
                    Console.WriteLine("{0} group {1} would deal defending group {2} {3} damage",
                        head, group.id, toAttack.id, damageDelt);
                }
            }
            List<Group> selectedGroups = allTeams.Select(x => x.selectedTarget).ToList();
            foreach (Group other in otherTeam)
            {
                if (!selectedGroups.Contains(other))
                {
                    int i = other.immuneTo.Contains(group.attackType) ? 0 : 1;
                    int w = other.weakTo.Contains(group.attackType) ? 2 : 1;
                    int damage = group.EffectivePower * i * w;
                    if ((damage > 0) && (damage > damageDelt))
                    {
                        ChoseTarget(other, damage);
                    }
                    else if (damage == damageDelt)
                    {
                        if (other.EffectivePower > defendingEffectivePower)
                        {
                            ChoseTarget(other, damage);
                        }
                        else if (other.EffectivePower == defendingEffectivePower)
                        {
                            if (other.initiative > defendersInitiative)
                            {
                                ChoseTarget(other, damage);
                            }
                        }
                    }
                }
            }
            return toAttack;
        }

        static List<Group> GetLiveGroups(bool isImmuneSystem, List<Group> allTeams)
        {
            return allTeams.Where(x => (x.isImmuneSystem == isImmuneSystem) && (x.units > 0)).ToList();
        }

        static void DumpTeamUnits(List<Group> team)
        {
            for (int i = 0; i < team.Count; i++)
            {
                Console.WriteLine("Group {0} contains {1} units", team[i].id, team[i].units);
            }
        }

        static List<Group> PerformBattle(int immuneBoost, out bool remi, bool verbose)
        {
            List<Group> homeTeam = ReadInput("Immune System:", true);
            foreach (Group g in homeTeam)
            {
                g.attackDamage += immuneBoost;
            }
            List<Group> awayTeam = ReadInput("Infection:", false);
            if (verbose)
            {
                SaveTeam(homeTeam, "Immune System:", false);
                SaveTeam(awayTeam, "Infection:", true);
            }
            List<Group> allTeams = new List<Group>(homeTeam);
            allTeams.AddRange(awayTeam);
            bool done = false;
            int homePointsLast = 0;
            int awayPointsLast = 0;
            remi = false;
            while (!done)
            {
                allTeams = allTeams.Where(x => x.units > 0).ToList();
                allTeams = allTeams.OrderBy(x => x.isImmuneSystem).ThenBy(x => x.id).ToList();
                if (verbose)
                {
                    Console.WriteLine("Immune System:");
                    DumpTeamUnits(GetLiveGroups(true, allTeams));
                    Console.WriteLine("Infection:");
                    DumpTeamUnits(GetLiveGroups(false, allTeams));
                    Console.WriteLine("");
                }
                allTeams = allTeams.OrderByDescending(x => x.EffectivePower).ThenByDescending(x => x.initiative).ToList();
                foreach (Group g in allTeams)
                {
                    g.selectedTarget = null;
                }
                foreach (Group g in allTeams)
                {
                    g.selectedTarget = SelectTarget(g, allTeams, verbose);
                }
                if (verbose)
                    Console.WriteLine();
                allTeams = allTeams.OrderByDescending(x => x.initiative).ToList();
                foreach (Group g in allTeams)
                {
                    if ((g.units > 0) && (g.selectedTarget != null) && (g.selectedTarget.units > 0))
                    {
                        PerformAttack(g, g.selectedTarget, verbose);
                    }
                }
                if (verbose)
                    Console.WriteLine();
                int homePoints = GetLiveGroups(true, allTeams).Select(x => x.units).Sum();
                int awayPoints = GetLiveGroups(false, allTeams).Select(x => x.units).Sum();
                remi = (homePoints == homePointsLast) && (awayPoints == awayPointsLast);
                done = (homePoints == 0) || (awayPoints == 0) || remi;
                homePointsLast = homePoints;
                awayPointsLast = awayPoints;
            }
            return allTeams.Where(x => x.units > 0).ToList();
        }

        static void PartA()
        {
            bool remi = false;
            List<Group> groupsLeft = PerformBattle(0, out remi, false);
            //List<Group> groupsLeft = PerformBattle(1570, out remi, true);
            string head = groupsLeft.First().isImmuneSystem ? "Immune System" : "Infection";
            int u = groupsLeft.Select(x => x.units).Sum();
            Console.WriteLine("Part A: {0} wins with {1} points.", head, u);
        }

        static void PartB()
        {
            List<Group> groupsLeft;
            int immuneBoost = 1;
            bool remi = false;
            do
            {
                groupsLeft = PerformBattle(immuneBoost++, out remi, false);
                Console.Write(".");
            }
            while (!groupsLeft.First().isImmuneSystem || remi);
            Console.WriteLine();
            int u = groupsLeft.Select(x => x.units).Sum();
            Console.WriteLine("Part B: Immune System wins with {0} points (using a boost of {1}).", 
                u, immuneBoost - 1);
        }

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day24).Namespace + ":");
            PartA();
            PartB();
        }
    }
}
