#include <iostream>
#include <vector>
#include <set>
int main() {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<int> input;
    {
        bool done = false;
        int sign = 0;
        int tmp = 0;
        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                tmp *= 10;
                tmp += c - '0';
            }
            else if (c == '-') sign = -1;
            else if (c == '+') sign = 1;
            else if (c == '\n' && sign) {
                ans1 += sign * tmp;
                input.push_back(sign*tmp);
                tmp = 0;
                sign = 0;
            }

        }
    }

    {
        bool found = false;
        std::set<int> seen;
        int tmp = 0;
        while (!found) {
            for (auto d : input) {
                tmp += d;
                if (seen.count(tmp)) {
                    ans2 = tmp;
                    found = true;
                    break;
                }
                seen.insert(tmp);
            }
        }
    }
    
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
