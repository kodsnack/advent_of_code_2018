#include <iostream>
#include <vector>
#include <array>

int main() {
    int ans1 = 0;
    std::string ans2 = "-";
    std::vector<std::array<int, 4>> input;
    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        bool neg = false;
        std::array<int, 4> tmp;
        int ant = 0;
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
            } else if(c == '-') {
                neg = true;
            } else {
                if(have_num) {
                    tmp[ant++] = num * (neg ? -1 : 1);
                }
                if(c == '\n') {
                    if(ant == 4) {
                        input.push_back(tmp);
                    }
                    ant = 0;
                }
                have_num = false;
                num = 0;
                neg = false;
            }

        }
    }

    std::vector<std::vector<std::array<int,4>>> co;
    for(auto & s : input) {
        co.emplace_back();
        co.back().push_back(s);
    }

    bool done = false;
    while(!done) {
        done = true;
        for (auto it1 = co.begin(); it1 != co.end(); it1++) {
            if(it1->empty()) continue;
            for (auto it2 = co.begin(); it2 != co.end(); it2++) {
                if (it1 == it2 || it2->empty()) continue;
                bool found = false;
                for (auto &s1 : *it1) {
                    for (auto &s2 : *it2) {
                        int dist = 0;
                        for (int i = 0; i < 4; i++) dist += abs(s1[i] - s2[i]);
                        if (dist <= 3) {
                            found = true;
                            break;
                        }
                    }
                    if(found) break;
                }
                if (found) {
                    it1->reserve(it1->size() + it2->size());
                    for (auto &s : *it2) {
                        it1->push_back(s);
                    }
                    it2->clear();
                    done = false;
                }
            }
        }
        co.erase(std::remove_if(co.begin(), co.end(), [](const auto & v) { return v.empty(); }), co.end());
    }

    ans1 = co.size();

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
