using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace day02
{
    class Day02
    {
        public static readonly string[] input =
        {
            "cvfueihajytpmrdkgsxfqplbxn", "cbzueihajytnmrdkgtxfqplbwn", "cvzucihajytomrdkgstfqplqwn", "cvzueilajytomrdkgsxfqwnbwn", "cvzueihajytomrdkgsgwqphbwn",
            "wuzuerhajytomrdkgsxfqplbwn", "cyzueifajybomrdkgsxfqplbwn", "cvzueihajxtomrdkgpxfqplmwn", "ivzfevhajytomrdkgsxfqplbwn", "cvzueihajytomrdlgsxfqphbbn",
            "uvzueihajjtomrdkgsxfqpobwn", "cvzupihajytomrdkgsxfqplpwe", "cvzueihajyvomrdkgsxfqplbrl", "cczueihajytomrdkgsnfqpxbwn", "cvzueigajytdmrdkgsxyqplbwn",
            "cvzujihljytomrdkgsxuqplbwn", "cvzueisajytomrddgsxkqplbwn", "cvzneihajytomrdkgsgaqplbwn", "cvzueihajytomrdkgsinmplbwn", "cveueihajyromrdkgsxfqplown",
            "cypueihajytotrdkgzxfqplbwn", "cvzuoihajytomvdqgsxfqplbwn", "cvzuekhejytwmrdkgsxfqplbwn", "cvzseihajytomrdkgsxfqgmbwn", "cvfuhihajytomrdkgsxfqplbwi",
            "cvzueihujxtomrdkgsufqplbwn", "cvzueihdjytomrdogsxfqplbwh", "cvzueihdjyfohrdkgsxfqplbwn", "cvtudihajytolrdkgsxfqplbwn", "cvzueihajytymrdkgshzqplbwn",
            "cvzuebhajytomxdkgsxfqplbwt", "cvzulihajyxomrdkgsbfqplbwn", "cvzueihajywomrdkgsxfqplbts", "cvzueihajytouodkdsxfqplbwn", "cvzueihajytomgdkgqxfqklbwn",
            "cvzubihajytomvdkgsxfqplmwn", "cvhueihajyyocrdkgsxfqplbwn", "zvzueihajytourdkgsxflplbwn", "cvzbeihajytomadkgsxfoplbwn", "cvzueihajytomrdkgnxfqplbsl",
            "cvfueihajftkmrdkgsxfqplbwn", "cvzuexhajytomryugsxfqplbwn", "cvzueihajytomsckgsxfqalbwn", "cvzuexhajytomrdkbsxfqpluwn", "cvzueihajytbmrtkgsxwqplbwn",
            "cvzueihajytomrdigsxfqqlbsn", "cvzweihajytomydkgsxfmplbwn", "bvzteihajytimrdkgsxfqplbwn", "cvzueihajytpmrdkgsxfcpbbwn", "cvzueigsjltomrdkgsxfqplbwn",
            "cvzueihajytomrikgsxfopldwn", "cvzueihajstomrdkgsxfqplgon", "cvzueimajytomrnkxsxfqplbwn", "cvzleihagatomrdkgsxfqplbwn", "cvbueihajotomrdkgsxfqjlbwn",
            "cvzueihajytomrdkgsxfqppnvn", "hvzueihajytomrdkghxfkplbwn", "cvzueigajytxmrdkgsxfqplbjn", "cvzueihaaxtokrdkgsxfqplbwn", "cvzueihajyeomrdkgujfqplbwn",
            "cvzueiwajpoomrdkgsxfqplbwn", "cvzieidtjytomrdkgsxfqplbwn", "cvzueihalytomrakbsxfqplbwn", "wtzueihajytomrdkgsxfqplbwq", "cvzuelhaiytomrdkgsxfqplcwn",
            "cvzueihajytomrdkgsxfqslswd", "cvzueihajytomrykgssfqplbon", "cvzueihfjytovrdegsxfqplbwn", "cvzueihajytomldqgsxfqplbwy", "cvzleihjjytomrtkgsxfqplbwn",
            "cvzueihaldtomrdtgsxfqplbwn", "cvzueihajytzmrdkgsxfeplqwn", "cvzueihrjytomddkgsxfqpgbwn", "cyzulihajytokrdkgsxfqplbwn", "cvsueihajytoordfgsxfqplbwn",
            "fvzueyhajytomrdkgaxfqplbwn", "cczueihajytobrdkgsefqplbwn", "cvzueihajytomcdrgscfqplbwn", "cvzuexhajyvomrdkgssfqplbwn", "cvzsmihajyiomrdkgsxfqplbwn",
            "cvzzeihajttomrdkgsxzqplbwn", "cvzseihajytomrdkgsxfqpebvn", "cvzueihajgthmrdkgsbfqplbwn", "ruzueihajytomrdkgsxfqphbwn", "cvzueihajytofrdkgsnfrplbwn",
            "cvzuetdajytojrdkgsxfqplbwn", "fvzueihajytomrdkghxfqpobwn", "cvzueihsjytomrdkgsxfqglbxn", "cvzueihajytowrdkgsxfqpsbun", "cvzteihaiytomrdkfsxfqplbwn",
            "cvzueihajytkmrdkrsxfqplvwn", "cvzueihajyoomrdkasxfqjlbwn", "lvzurihajytkmrdkgsxfqplbwn", "cvzueihajyyomrdagsxfqelbwn", "cvfueihajytomrdkgsxfqplbbx",
            "cvwueihajytommdkgkxfqplbwn", "cvzucicajytomrdkgsxcqplbwn", "dvzueihahytgmrdkgsxfqplbwn", "cvzuechajytomrdkgsxfqelwwn", "cvzuekhajytomrdkgsxknplbwn",
            "cvtueihajytomphkgsxfqplbwn", "cvzueihabytnzrdkgsxfqplbwn", "cvzusihajytomrdkgfxfqplban", "cvfueihajytomcdfgsxfqplbwn", "mvzueihapytomrdkgsxfdplbwn",
            "cvzueihajytomhdkgsxmqppbwn", "jvsueihajytomrdkgsxfqplbln", "cvzujihajybomrdkgsxtqplbwn", "cvzuekhawytomrdkgsxfqplbwc", "svzueihanytomrdogsxfqplbwn",
            "cvzujihajytodrdkgslfqplbwn", "cvgdeihajytorrdkgsxfqplbwn", "cvzbeihajytoprdkgsxfqplbyn", "cvzueihkyytomjdkgsxfqplbwn", "cvzuelhojytomrdkgsxfqjlbwn",
            "evzueihajytimrdkgsxfqpsbwn", "cvzueihajydomrdkjsxfqplbjn", "ovzteihajytosrdkgsxfqplbwn", "cvzueihajyaomrdzgsxfqplbgn", "cvzuewhajmtomrdkgsufqplbwn",
            "cvzueihajqtomhukgsxfqplbwn", "cvzueihajytomzqkgsxfqplbwk", "cazuewhakytomrdkgsxfqplbwn", "clzueihatytomrdkgzxfqplbwn", "dvzueihajytomqdkgsxfqpnbwn",
            "cvzueidajdtomrdkgsxtqplbwn", "cvzueihabytowrdkgsxoqplbwn", "cvzujihwjytomrdkgsxeqplbwn", "cvtuedhajytomrdkgsxfqplbbn", "cvzueihajcgomrdkgsxfqplswn",
            "cvzuephajyiomrdngsxfqplbwn", "cvzueihajythmqdkgsxfqplbwf", "cvzueitajytomrdkgsxfepvbwn", "cvzueihajytomydkgsxfqplvwb", "dvzueshajytomrddgsxfqplbwn",
            "cvzueihajytomrdkgvxfqpwben", "cvzueihajytomrdkgvxfpplwwn", "cvzuefhajftomrdkgsxfqrlbwn", "cvzueihajytpmrvkgsxfqplbcn", "cvzueihajytohrdkgsxfqxnbwn",
            "cvzueihajytomrdposxfqulbwn", "cozueihajytomrpkgsxfqrlbwn", "cvzuuihaxytomrdkgsxfqplbtn", "cvzueihajytomrbzgsxyqplbwn", "cveueihajyxoqrdkgsxfqplbwn",
            "cvzueihajytomrkkgsxfqptbrn", "cvzuezhajatomrdkssxfqplbwn", "cpzueihajytomrdkgsxfhplbwo", "lviueihajytomrekgsxfqplbwn", "cvzueihwjytomrdkusxfyplbwn",
            "cvzgeihajytomwdkgsxfrplbwn", "cvzsejhzjytomrdkgsxfqplbwn", "cvzuuihajytomrdkgsxfqdlbwz", "cvzjeihajytomrdugsxftplbwn", "cvzueihaxytomrrkgsxfmplbwn",
            "cvzueihajgtomrdhgsxfqplwwn", "cvzulihajytomedkgsxfqplewn", "cvzueivajytomrdkmsxfqplbwc", "cvzuervajytomrdkgsxfwplbwn", "cvzuemhcjytomrdkgslfqplbwn",
            "cvzyerhauytomrdkgsxfqplbwn", "cvzueihaoytomrdkgsyfqplewn", "cvzueihanytomrdkgsafkplbwn", "cvzueihajvtomrdugsxfqpcbwn", "chzueihajytamrdxgsxfqplbwn",
            "cvzueihalytomrdsgsxfqplbln", "cvzueihajytoyaykgsxfqplbwn", "tlzueihajyeomrdkgsxfqplbwn", "cvpueihajytbmrdkgsxfxplbwn", "cvzueihajytomjdkgsxuqplkwn",
            "cvzueihajygomrdkgkxfqplbwg", "cvzueihajhtomrdkgbxsqplbwn", "cvzurihajytomrdkgsafqplbwx", "cdzuezhajytomrdkgsxrqplbwn", "cvbueihajytotrwkgsxfqplbwn",
            "cwzkeihajytomrdkgsxfqplbwh", "cvzheihajytolrikgsxfqplbwn", "cozuevhajytomrdkgkxfqplbwn", "chzueihajytomrjkgsxfqulbwn", "cvzueihkjyromrdkgsxvqplbwn",
            "cvzveihajytomrdkgsxpqplnwn", "cvzueihajytoirdkgsxfqihbwn", "cvoueihajytomrdkgsxfqpdawn", "pvzueihajytomrdkgnxfqplbfn", "cvzueihakytomxdkgssfqplbwn",
            "cvzueivajytomrdbgsxaqplbwn", "cvzueihajytokrdkgszrqplbwn", "cvzuevhajytomrdkgsxgqplbwi", "cvzueihajylomrdkgsxflplbpn", "hvzueihajytomvdkgsxfqplgwn",
            "cvzleihajytymrrkgsxfqplbwn", "crzueieajytomrdkgsxfqplbon", "cszueihajytomrdlgqxfqplbwn", "cvzueihacytomrdkgsxfjblbwn", "cvzreihajytomrdkgsxfqplzun",
            "cvzurihajytomrdkgsxiqplawn", "uvzueihajyhovrdkgsxfqplbwn", "cvzueihajyqodrdkgssfqplbwn", "cvzwiihrjytomrdkgsxfqplbwn", "cqzueihajytomrdkgjxfqplban",
            "cvmueihajytoordkgsxfqplbyn", "cypueihajytomrdkgzxfqplbwn", "cvzueihajykomrdkgsmfqplbtn", "cvzueidajytimrdkgsxfqpdbwn", "cvzheihajytomrdkgsxfqpfewn",
            "dvzueihajytumrdzgsxfqplbwn", "cvzueixajytomrdkgsvfqplgwn", "cvzuevhzjyzomrdkgsxfqplbwn", "cvyeeihajytomrdkgsxnqplbwn", "cvzueihajytomrdkggtpqplbwn",
            "cvzceiyajytomrdkgexfqplbwn", "cvzuelhajyyomrdkzsxfqplbwn", "cvzhzihajygomrdkgsxfqplbwn", "cvzueihwjytomrdkgsgfqplbrn", "cvzsevhajytomrdkgqxfqplbwn",
            "cvzueiuajytomrdkgsxfppebwn", "nvzueihajytemrdkgsxwqplbwn", "cvzueihajytocgdkgsxfqvlbwn", "cczusihajytomrdkgsxfqplbpn", "cmzueihajytomrdkbsxwqplbwn",
            "cvzumfdajytomrdkgsxfqplbwn", "cvzueihcjytomrdkgsxfqplbkl", "cvzueihajytomawknsxfqplbwn", "kvzueihijytomrdkgsxdqplbwn", "cdzutihajytomrdkgsxfkplbwn",
            "cvzufihadylomrdkgsxfqplbwn", "cvzueihajytomrgkxsxfqphbwn", "cvzuewhajyzomrdkgsxfqelbwn", "cvzueihajytomrdkgqxfqelbwc", "cvzueshajyoomrdkgsxfqflbwn",
            "cvzueihajyromrekgixfqplbwn", "chzugihajytomrdkgsxfqplawn", "cvzueihajytomrdkgsxfhpmbwy", "cvzueihacytodxdkgsxfqplbwn", "cvzurihajytourdkgsdfqplbwn",
            "cvzzeihmjytomrddgsxfqplbwn", "cvzucyhajygomrdkgsxfqplbwn", "ckzueihzjytomrdkgsxwqplbwn", "cvlueihajmtozrdkgsxfqplbwn", "cvzkeihajytomrdkgsxfqclbwc",
            "cvzueihajytomrdkgsxgdplbwa", "cvzueihyjytoxrdkgcxfqplbwn", "cvzueizavytomfdkgsxfqplbwn", "cvzueihajwtosrdkgsxfqllbwn", "cvzueihajytomrdaksxfqpllwn",
            "cvzuuihojytombdkgsxfqplbwn", "cvzuiibajytpmrdkgsxfqplbwn", "cvzueihajyuomydkgsxfqplzwn", "cvzueihajytimrmkgsxfqplfwn", "cvzueihajytomrdkgzxfqpljwo",
        };

        static void Main(string[] args)
        {
            Console.WriteLine("AoC 2018 - " + typeof(Day02).Namespace + ":");

            // Part A
            int nInput = input.Length;
            int nTwice = 0;
            int nThrice = 0;
            foreach (string s in input)
            {
                int hasTwice = 0;
                int hasThrice = 0;
                Dictionary<char, int> d = new Dictionary<char, int>();
                foreach (char c in s)
                {
                    d[c] = d.ContainsKey(c) ? d[c] + 1 : 1;
                }
                foreach (KeyValuePair<char, int> kvp in d)
                {
                    if (kvp.Value == 2)
                    {
                        hasTwice = 1;
                    }
                    else if (kvp.Value == 3)
                    {
                        hasThrice = 1;
                    }
                }
                nTwice += hasTwice;
                nThrice += hasThrice;
            }
            Console.WriteLine("Part A: Box IDs checksum is " + nTwice + " * " + nThrice + " = " + nTwice * nThrice + ".");

            // Part B
            string FindCommonLetters()
            {
                int nChars = input[0].Length;
                for (int a = 0; a < nInput - 1; a++)
                {
                    for (int b = a + 1; b < nInput; b++)
                    {
                        StringBuilder commonChars = new StringBuilder();
                        for (int i = 0; i < nChars; i++)
                        {
                            if (input[a][i] == input[b][i])
                            {
                                commonChars.Append(input[a][i]);
                            }
                        }
                        string commonCharsTotal = commonChars.ToString();
                        if (commonCharsTotal.Length == nChars - 1)
                        {
                            return commonCharsTotal;
                        }
                    }
                }
                return "";
            }
            string id = FindCommonLetters();
            Console.WriteLine("Part B: Common chars are '" + id + "'.");
        }
    }
}
