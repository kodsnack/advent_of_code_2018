#include <iostream>
#include <vector>
#include <string>
#include <climits>

int main() {
    int ans1 = 0;
    int ans2 = 0;

    std::string input;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;

        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                input.push_back(c);
            }

        }
    }

    auto calc = [](const std::string & in, char skip = ' ')
    {
        std::string out;
        out.reserve(in.length());
        auto skiphi = toupper(skip);
        for(const auto & c : in) {
            if(c == skip || c == skiphi) continue;
            if(out.empty()) {
                out.push_back(c);
                continue;
            }

            if((islower(out.back()) && isupper(c) && out.back() == tolower(c)) ||
               (isupper(out.back()) && islower(c) && out.back() == toupper(c))) {
                out.pop_back();
            } else {
                out.push_back(c);
            }
        }
        return out;
    };

    auto reduced = calc(input);
    ans1 = reduced.length();

    ans2 = INT_MAX;

    for(char c = 'a'; c <= 'z'; c++) {
        auto out = calc(reduced, c);
        auto tmp = out.length();
        if(tmp < ans2) ans2 = tmp;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
