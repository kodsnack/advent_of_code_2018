#include <iostream>
#include <vector>
#include <tuple>
#include <map>
#include <set>

int main() {
    std::string ans1;
    int ans2 = 0;

    std::vector<std::tuple<char,char>> input;

    {
        bool done = false;
        char s1 = 0, s2 = 0;
        bool have_s = false;
        bool have_1 = false, have_2 = false;
        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c >= 'A' && c <= 'Z') {
                if(!have_s) {
                    have_s = true;
                } else {
                    if(!have_1) {
                        s1 = c;
                        have_1 = true;
                    } else {
                        s2 = c;
                        have_2 = true;
                    }
                }
            } else {
                if(c == '\n') {
                    if(have_1 && have_2) {
                        input.emplace_back(s1,s2);
                    }
                    s1 = s2 = 0;
                    have_1 = have_2 = false;
                    have_s = false;
                }
            }

        }
    }

    std::map<char, std::set<char>> requirements;
    for (auto[a, b] : input) {
        if (!requirements.count(a)) requirements[a] = std::set<char>();
        requirements[b].insert(a);
    }

    {
        auto reqs = requirements;

        while (!reqs.empty()) {
            char c = 0;
            for (const auto & p : reqs) {
                if (p.second.empty()) {
                    c = p.first;
                    break;
                }
            }
            reqs.erase(c);
            ans1.push_back(c);
            for (auto &p : reqs) {
                p.second.erase(c);
            }
        }
    }

    {
        auto reqs = requirements;
        const int workers = 5;
        const int penalty = 60;

        std::map<char, int> working;

        while (!reqs.empty() || !working.empty()) {
            // check for new workers to start
            for(auto it = reqs.begin(); it != reqs.end(); ) {
                if(it->second.empty() && working.size() < workers) {
                    working[it->first] = penalty + 1 + it->first - 'A';
                    it = reqs.erase(it);
                } else {
                    it++;
                }
            }

            // do work and check for done
            auto timestep = std::min_element(working.begin(), working.end(),
                    [](const auto & p1, const auto & p2) {return p1.second < p2.second; })->second;

            for(auto it = working.begin(); it != working.end(); ) {
                it->second-=timestep;
                if(!it->second) {
                    for(auto & p : reqs) {
                        p.second.erase(it->first);
                    }

                    it = working.erase(it);

                } else {
                    it++;
                }
            }
            ans2+=timestep;
        }

    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
