#include <iostream>
#include <vector>
#include <array>

enum class ops {
    addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr
};

struct step {
    ops op;
    int a0, a1, a2;
    step(ops _o, int _a0, int _a1, int _a2) : op{_o}, a0{_a0}, a1{_a1}, a2{_a2} {}
};

int main() {
    int ans1 = 0;
    int ans2 = 0;
    int ip = 0;
    std::vector<step> program;
    {
        bool done = false;
        int num = 0;
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
    for(int i = 0; i < 2; i++) {
        std::array<int, 6> reg = {0,};
        reg[0] = i;
        int varv = 0;
        while (reg[ip] < (int) program.size() && varv < 100) {
            varv++;
            const auto &cmd = program[reg[ip]];
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

        auto maxreg = *std::max_element(reg.begin(), reg.end());

        int ans = 0;
        for(int d = 1; d*d<=maxreg; d++) {
            if(maxreg%d==0) ans += d+maxreg/d;
        }

        (i ? ans2 : ans1) = ans;

    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
