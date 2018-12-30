#include <iostream>
#include <vector>
#include <array>
#include <stdint.h>
#include <set>
enum class ops {
    addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
};

struct step {
    ops op;
    int64_t a0, a1, a2;
    step(ops _o, int64_t _a0, int64_t _a1, int64_t _a2) : op{_o}, a0{_a0}, a1{_a1}, a2{_a2} {}
};

int main() {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    int ip = 0;
    std::vector<step> program;
    {
        bool done = false;
        int64_t num = 0;
        bool have_num = false;
        std::string cmd;
        std::vector<int> nums;
        bool comment = false;
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
            } else if(c >= 'a' && c <= 'z') {
                if(!comment) cmd.push_back(c);
            } else if(c == '!') {
                comment = true;
            } else {
                if(have_num && !comment) {
                    nums.push_back(num);
                }
                if(c == '\n') {
                    if(cmd == "ip") { ip = nums.front(); }
                    else if(nums.size() == 3) {
                        //     addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
                        if(cmd == "addr") program.emplace_back(ops::addr, nums[0], nums[1], nums[2]);
                        if(cmd == "addi") program.emplace_back(ops::addi, nums[0], nums[1], nums[2]);
                        if(cmd == "mulr") program.emplace_back(ops::mulr, nums[0], nums[1], nums[2]);
                        if(cmd == "muli") program.emplace_back(ops::muli, nums[0], nums[1], nums[2]);
                        if(cmd == "banr") program.emplace_back(ops::banr, nums[0], nums[1], nums[2]);
                        if(cmd == "bani") program.emplace_back(ops::bani, nums[0], nums[1], nums[2]);
                        if(cmd == "borr") program.emplace_back(ops::borr, nums[0], nums[1], nums[2]);
                        if(cmd == "bori") program.emplace_back(ops::bori, nums[0], nums[1], nums[2]);
                        if(cmd == "setr") program.emplace_back(ops::setr, nums[0], nums[1], nums[2]);
                        if(cmd == "seti") program.emplace_back(ops::seti, nums[0], nums[1], nums[2]);
                        if(cmd == "gtir") program.emplace_back(ops::gtir, nums[0], nums[1], nums[2]);
                        if(cmd == "gtri") program.emplace_back(ops::gtri, nums[0], nums[1], nums[2]);
                        if(cmd == "gtrr") program.emplace_back(ops::gtrr, nums[0], nums[1], nums[2]);
                        if(cmd == "eqir") program.emplace_back(ops::eqir, nums[0], nums[1], nums[2]);
                        if(cmd == "eqri") program.emplace_back(ops::eqri, nums[0], nums[1], nums[2]);
                        if(cmd == "eqrr") program.emplace_back(ops::eqrr, nums[0], nums[1], nums[2]);
                    }
                    cmd.clear();
                    nums.clear();
                    comment = false;
                }
                have_num = false;
                num = 0;
            }

        }
    }

    // heuristics:
    // run program 100 steps, take largest register value,
    // answer is sum of divisors
    std::set<int64_t> seen;
    bool slow = false;
    if(slow) {
        std::array<int64_t, 6> reg = {0,};
        while (reg[ip] < (int) program.size()) {
            const auto &cmd = program[reg[ip]];
            if (cmd.op == ops::eqrr && (cmd.a0 == 0 || cmd.a1 == 0)) {
                int r = (cmd.a0 == 0 ? cmd.a1 : cmd.a0);
                if (seen.empty()) ans1 = reg[r];
                if (seen.find(reg[r]) != seen.end()) break;
                seen.insert(reg[r]);
                ans2 = reg[r];
            }
            switch (cmd.op) {
                case ops::addr:
                    reg[cmd.a2] = reg[cmd.a0] + reg[cmd.a1];
                    break;
                case ops::addi:
                    reg[cmd.a2] = reg[cmd.a0] + cmd.a1;
                    break;

                case ops::mulr:
                    reg[cmd.a2] = reg[cmd.a0] * reg[cmd.a1];
                    break;
                case ops::muli:
                    reg[cmd.a2] = reg[cmd.a0] * cmd.a1;
                    break;

                case ops::banr:
                    reg[cmd.a2] = (reg[cmd.a0] & reg[cmd.a1]);
                    break;
                case ops::bani:
                    reg[cmd.a2] = (reg[cmd.a0] & cmd.a1);
                    break;

                case ops::borr:
                    reg[cmd.a2] = (reg[cmd.a0] | reg[cmd.a1]);
                    break;
                case ops::bori:
                    reg[cmd.a2] = (reg[cmd.a0] | cmd.a1);
                    break;

                case ops::setr:
                    reg[cmd.a2] = reg[cmd.a0];
                    break;
                case ops::seti:
                    reg[cmd.a2] = cmd.a0;
                    break;

                case ops::gtir:
                    reg[cmd.a2] = (cmd.a0 > reg[cmd.a1] ? 1 : 0);
                    break;
                case ops::gtri:
                    reg[cmd.a2] = (reg[cmd.a0] > cmd.a1 ? 1 : 0);
                    break;
                case ops::gtrr:
                    reg[cmd.a2] = (reg[cmd.a0] > reg[cmd.a1] ? 1 : 0);
                    break;

                case ops::eqir:
                    reg[cmd.a2] = (cmd.a0 == reg[cmd.a1] ? 1 : 0);
                    break;
                case ops::eqri:
                    reg[cmd.a2] = (reg[cmd.a0] == cmd.a1 ? 1 : 0);
                    break;
                case ops::eqrr:
                    reg[cmd.a2] = (reg[cmd.a0] == reg[cmd.a1] ? 1 : 0);
                    break;
            }
            reg[ip]++;
        }
    } else {
        int64_t lg = 0, sm = 0;
        // find numbers from data
        for(const auto & c : program) {
            if(c.op == ops::muli && c.a1 > 256) sm = c.a1;
            if(c.op == ops::seti && c.a0 > 1024) lg = c.a0;
        }

        if(!(lg && sm)) {
            std::cout << "magic numbers not found!" << std::endl;
            return 0;
        }

        int64_t r1 = 0, r2 = 0, r5 = 0;

        while(1) {
            r2 = 0x10000 | r1;
            r1 = lg;
            while(r2) {
                r5 = r2 & 0xff;
                r1 = (r1 + r5) & 0xffffff;
                r1 = (r1 * sm) & 0xffffff;
                r2 /= 256;
            }
            if(seen.empty()) ans1 = r1;
            if(seen.find(r1) != seen.end()) break;
            ans2 = r1;
            seen.insert(r1);
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
