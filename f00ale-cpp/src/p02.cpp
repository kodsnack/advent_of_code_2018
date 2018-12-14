#include <iostream>
#include <vector>
#include <array>
#include <algorithm>
#include <string>
int main() {
    int ans1 = 1;
    std::string ans2;

    std::vector<std::string> input;
    std::vector<int> ns(100);

    {
        bool done = false;
        std::array<int, 'z'-'a'+1> v = {0,};
        std::string s;
        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if (c >= 'a' && c <= 'z') {
                v[c - 'a']++;
                s.push_back(c);
            } else if (c == '\n') {
                std::sort(v.rbegin(), v.rend());
                auto it = v.begin();
                while(*it > 1) {
                    auto n = *it;
                    ns[n]++;
                    do {
                        it++;
                    } while(*it == n);
                }

                for (auto &i : v) {
                    i = 0;
                }

                if (!s.empty()) {
                    input.push_back(s);
                    s.clear();
                }
            }
        }
    }

    for (auto i : ns) if(i) { ans1 *= i; }

    for (auto it1 = input.begin(); it1 != input.end(); it1++) {
        for (auto it2 = it1 + 1; it2 != input.end(); it2++) {
            int diff = 0;

            for (size_t i = 0; i < it1->length(); i++) {
                if (it1->at(i) != it2->at(i)) {
                    diff++;
                    if (diff > 1) break;
                }
            }

            if (diff == 1) {
                for (size_t i = 0; i < it1->length(); i++) {
                    if (it1->at(i) == it2->at(i)) {
                        ans2.push_back(it1->at(i));
                    }
                }
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
