using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace day04
{
    class Program
    {
        public struct GuardNote
        {
            public int id;
            public bool beginsShift;
            public bool fallsAsleep;
            public bool wakesUp;
            public int startMinute;
        };

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Program).Namespace + ":");

            // First read input
            SortedDictionary<DateTime, string> guardNotesRaw = new SortedDictionary<DateTime, string>();
            Regex parts = new Regex(@"^\[(\d+)-(\d+)-(\d+) (\d+)\:(\d+)\] (.*)");
            string path = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), @"..\..\input.txt");
            StreamReader reader = File.OpenText(path);
            string line;
            while ((line = reader.ReadLine()) != null)
            {
                MatchCollection matches = parts.Matches(line);
                if (matches.Count > 0)
                {
                    GroupCollection groups = matches[0].Groups;
                    int i = 1;
                    int y = int.Parse(groups[i++].Value);
                    int m = int.Parse(groups[i++].Value);
                    int d = int.Parse(groups[i++].Value);
                    int hour = int.Parse(groups[i++].Value);
                    int minute = int.Parse(groups[i++].Value);
                    string s = groups[i++].Value;
                    DateTime date = new DateTime(y, m, d, hour, minute, 0);
                    guardNotesRaw.Add(date, s);
                }
            }
            if (guardNotesRaw.Count != 1064)
            {
                return;
            }

            // Part A
            SortedDictionary<DateTime, GuardNote> guardNotes = new SortedDictionary<DateTime, GuardNote>();
            int lastId = -1;
            foreach (KeyValuePair<DateTime, string> kvp in guardNotesRaw)
            {
                GuardNote note = new GuardNote();
                note.startMinute = kvp.Key.Minute;
                if (kvp.Value.StartsWith("falls asleep"))
                {
                    note.fallsAsleep = true;
                }
                else if (kvp.Value.StartsWith("wakes up"))
                {
                    note.wakesUp = true;
                }
                else if (kvp.Value.StartsWith("Guard"))
                {
                    note.beginsShift = true;
                    string id = new string(kvp.Value.Where(char.IsDigit).ToArray());
                    lastId = int.Parse(id);
                    if (kvp.Key.Hour != 0)
                    {
                        note.startMinute = 0;
                    }
                }
                note.id = lastId;
                guardNotes.Add(kvp.Key, note);
            }
            Dictionary<int, int[]> guardMinutes = new Dictionary<int, int[]>();
            GuardNote lastNote = new GuardNote();
            foreach (KeyValuePair<DateTime, GuardNote> kvp in guardNotes)
            {
                GuardNote thisNote = kvp.Value;
                if (!guardMinutes.ContainsKey(thisNote.id))
                {
                    guardMinutes.Add(thisNote.id, new int[60]);
                }
                if ((lastNote.id == thisNote.id) &&
                    lastNote.fallsAsleep && thisNote.wakesUp)
                {
                    int[] sleepMinutes = guardMinutes[thisNote.id];
                    for (int i = lastNote.startMinute; i < thisNote.startMinute; i++)
                    {
                        sleepMinutes[i]++;
                    }
                    guardMinutes[thisNote.id] = sleepMinutes;
                }
                lastNote = thisNote;
            }
            int sleepiestId = -1;
            int maxTotalMinutes = -1;
            foreach (KeyValuePair<int, int[]> kvp in guardMinutes)
            {
                int totalMinutes = kvp.Value.Sum();
                if (totalMinutes > maxTotalMinutes)
                {
                    maxTotalMinutes = totalMinutes;
                    sleepiestId = kvp.Key;
                }
            }
            int[] finalMinutes = guardMinutes[sleepiestId];
            int mostAsleepMinute = -1;
            int mostAsleepAmount = -1;
            for (int i = 0; i < 60; i++)
            {
                if (finalMinutes[i] > mostAsleepAmount)
                {
                    mostAsleepAmount = finalMinutes[i];
                    mostAsleepMinute = i;
                }
            }
            Console.WriteLine("Part A: Guard #" + sleepiestId + " slept the most minute " + mostAsleepMinute +
                " --> " + sleepiestId * mostAsleepMinute + ".");

            // Part B
            int maxAmount = -1;
            int maxMinute = -1;
            int maxId = -1;
            foreach (KeyValuePair<int, int[]> kvp in guardMinutes)
            {
                for (int i = 0; i < 60; i++)
                {
                    if (kvp.Value[i] > maxAmount)
                    {
                        maxAmount = kvp.Value[i];
                        maxMinute = i;
                        maxId = kvp.Key;
                    }
                }
            }
            Console.WriteLine("Part B: Guard #" + maxId + " slept the most minute " + maxMinute +
                " --> " + maxId * maxMinute + ".");
        }
    }
}
