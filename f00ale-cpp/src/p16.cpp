#include <iostream>
#include <vector>
#include <array>
#include <algorithm>
#include <set>
int main() {
    int ans1 = 0;
    int ans2 = 0;

    std::vector<std::array<int, 12>> samples;

    std::vector<std::array<int, 4>> program;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::vector<int> tmp;
        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                have_num = true;
            } else {
                if(have_num) {
                    tmp.push_back(num);
                }
                if(c == ']') {
                    if(tmp.size() == 12) {
                        std::array<int, 12> a;
                        std::copy(tmp.begin(), tmp.end(), a.begin());
                        samples.emplace_back(a);
                        tmp.clear();
                    }
                }
                if(c == '\n' && done) {
                    std::array<int, 4> a;
                    for(size_t i = 0; i < tmp.size(); i+=4) {
                        std::copy_n(tmp.begin()+i, 4, a.begin());
                        program.emplace_back(a);
                    }
                }
                have_num = false;
                num = 0;
            }

        }
    }

    enum class ops {
        addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
    };

    std::vector<std::set<ops>> possible(16);
    for(auto & p : possible) {
        p = {ops::addr, ops::addi, ops::mulr, ops::muli, ops::banr, ops::bani, ops::borr, ops::bori,
             ops::setr, ops::seti, ops::gtir, ops::gtri, ops::gtrr, ops::eqir, ops::eqri, ops::eqrr};
    }

    for(const auto & s : samples) {
        std::array<int, 4> before;
        std::array<int, 4> cmd;
        std::array<int, 4> after;
        std::copy_n(s.begin(), 4, before.begin());
        std::copy_n(s.begin()+4, 4, cmd.begin());
        std::copy_n(s.begin()+8, 4, after.begin());

        std::set<ops> poss;

        if(after[cmd[3]] == before[cmd[1]]+before[cmd[2]]) { poss.emplace(ops::addr); }
        if(after[cmd[3]] == before[cmd[1]]+cmd[2]) { poss.emplace(ops::addi); }
        if(after[cmd[3]] == before[cmd[1]]*before[cmd[2]]) { poss.emplace(ops::mulr); }
        if(after[cmd[3]] == before[cmd[1]]*cmd[2]) { poss.emplace(ops::muli); }
        if(after[cmd[3]] == (before[cmd[1]]&before[cmd[2]])) { poss.emplace(ops::banr); }
        if(after[cmd[3]] == (before[cmd[1]]&cmd[2])) { poss.emplace(ops::bani); }
        if(after[cmd[3]] == (before[cmd[1]]|before[cmd[2]])) { poss.emplace(ops::borr); }
        if(after[cmd[3]] == (before[cmd[1]]|cmd[2])) { poss.emplace(ops::bori); }
        if(after[cmd[3]] == before[cmd[1]]) { poss.emplace(ops::setr); }
        if(after[cmd[3]] == cmd[1]) { poss.emplace(ops::seti); }

        if(after[cmd[3]] == (cmd[1]>before[cmd[2]]?1:0)) { poss.emplace(ops::gtir); }
        if(after[cmd[3]] == (before[cmd[1]]>cmd[2]?1:0)) { poss.emplace(ops::gtri); }
        if(after[cmd[3]] == (before[cmd[1]]>before[cmd[2]]?1:0)) { poss.emplace(ops::gtrr); }

        if(after[cmd[3]] == (cmd[1]==before[cmd[2]]?1:0)) { poss.emplace(ops::eqir); }
        if(after[cmd[3]] == (before[cmd[1]]==cmd[2]?1:0)) { poss.emplace(ops::eqri); }
        if(after[cmd[3]] == (before[cmd[1]]==before[cmd[2]]?1:0)) { poss.emplace(ops::eqrr); }

        if(poss.size() >= 3) ans1++;

        std::set<ops> n;
        std::set_intersection(possible[cmd[0]].begin(), possible[cmd[0]].end(),
                poss.begin(), poss.end(), std::inserter(n, n.end()));

        possible[cmd[0]].swap(n);
    }

    bool done = false;
    while(!done) {
        done = true;
        for(auto & s : possible) {
            if(s.size() != 1) { done = false; continue; }

            for(auto & s2 : possible) {
                if(&s == &s2) continue;
                s2.erase(*s.begin());
            }

        }
    }

    std::vector<ops> op;
    for(auto & p : possible) op.push_back(*p.begin());

    {
        std::array<int, 4> reg = {0,};

        for(auto & cmd : program) {
            switch(op[cmd[0]]) {
                case ops::addr: reg[cmd[3]] = reg[cmd[1]] + reg[cmd[2]]; break;
                case ops::addi: reg[cmd[3]] = reg[cmd[1]] + cmd[2]; break;

                case ops::mulr: reg[cmd[3]] = reg[cmd[1]] * reg[cmd[2]]; break;
                case ops::muli: reg[cmd[3]] = reg[cmd[1]] * cmd[2]; break;

                case ops::banr: reg[cmd[3]] = (reg[cmd[1]] & reg[cmd[2]]); break;
                case ops::bani: reg[cmd[3]] = (reg[cmd[1]] & cmd[2]); break;

                case ops::borr: reg[cmd[3]] = (reg[cmd[1]] | reg[cmd[2]]); break;
                case ops::bori: reg[cmd[3]] = (reg[cmd[1]] | cmd[2]); break;

                case ops::setr: reg[cmd[3]] = reg[cmd[1]]; break;
                case ops::seti: reg[cmd[3]] = cmd[1]; break;

                case ops::gtir: reg[cmd[3]] = (cmd[1] > reg[cmd[2]] ? 1 : 0); break;
                case ops::gtri: reg[cmd[3]] = (reg[cmd[1]] > cmd[2] ? 1 : 0); break;
                case ops::gtrr: reg[cmd[3]] = (reg[cmd[1]] > reg[cmd[2]] ? 1 : 0); break;

                case ops::eqir: reg[cmd[3]] = (cmd[1] == reg[cmd[2]] ? 1 : 0); break;
                case ops::eqri: reg[cmd[3]] = (reg[cmd[1]] == cmd[2] ? 1 : 0); break;
                case ops::eqrr: reg[cmd[3]] = (reg[cmd[1]] == reg[cmd[2]] ? 1 : 0); break;
            }
        }

        ans2 = reg[0];
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
