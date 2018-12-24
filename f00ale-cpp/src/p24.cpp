#include <iostream>
#include <vector>
#include <algorithm>
#include <tuple>
#include <set>

struct group {
    bool isinfection;
    int id;
    int units;
    int hp;
    std::set<std::string> weak;
    std::set<std::string> immune;
    std::string damagetype;
    int damage;
    int initiative;
    void reset() {
        units = hp = id = damage = initiative = 0;
        target = -1;
        isinfection = selected = false;
        weak.clear();
        immune.clear();
        damagetype.clear();
    }
    int effective() const {
        return damage*units;
    }
    bool selected;
    int target;
};

bool operator<(const group & u1, const group & u2) {
    if(u1.effective() == u2.effective()) return u1.initiative > u2.initiative;
    else return u1.effective() > u2.effective();
}

int main() {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<group> inputunits;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::string str, last;
        group u;
        u.reset();
        int idx = 0;
        bool weak = false, immune = false;
        bool does = false, init = false;
        bool isinfection = false;
        bool inparen = false;
        int id = 0;
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
            } else if((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')) {
                str.push_back(c);
            } else {
                if(c == '(') inparen = true;

                if(have_num) {
                    switch(idx) {
                        case 0: u.units = num; break;
                        case 1: u.hp = num; break;
                        case 2: u.damage = num; break;
                        case 3: u.initiative = num; break;
                    }
                    idx++;
                } else if(!str.empty()) {
                    if(inparen) {
                        if(str == "to") {}
                        else if(str == "weak") {
                            weak = true; immune = false;
                        } else if(str == "immune") {
                            weak = false; immune = true;
                        } else if(weak) {
                            u.weak.insert(str);
                        } else if(immune) {
                            u.immune.insert(str);
                        }
                    }
                    else if(str == "damage") {
                        u.damagetype = last;
                    }

                    if(str == "Infection") {
                        id = 0;
                        isinfection = true;
                    }
                }
                if(c == ')') inparen = false;

                if(c == '\n') {
                    if(idx) {
                        u.isinfection = isinfection;
                        id++;
                        u.id = id;
                        inputunits.push_back(u);
                    }

                    u.reset();

                    idx = 0;
                    weak = immune = does = init = false;
                    last.clear();
                    str.clear();
                }
                have_num = false;
                num = 0;
                last.swap(str);
                str.clear();
            }

        }
    }

    bool found = false;
    while(!found) {
        auto units = inputunits;

        while (std::count_if(units.begin(), units.end(), [](auto &u) { return u.isinfection; }) &&
               std::count_if(units.begin(), units.end(), [](auto &u) { return !u.isinfection; })) {
            std::sort(units.begin(), units.end());

            for (auto &u : units) {
                int maxdamage = 0;
                int maxindex = -1;
                int targ = 0;
                for (int i = 0; i < (int) units.size(); i++) {
                    auto &t = units[i];
                    if (u.isinfection == t.isinfection) continue;
                    if (t.selected) continue;

                    int modifier = t.weak.count(u.damagetype) ? 2 : 1;
                    modifier *= t.immune.count(u.damagetype) ? 0 : 1;

                    int damage = u.units * u.damage * modifier;

                    if (damage > maxdamage) {
                        maxdamage = damage;
                        maxindex = i;
                        targ = t.id;
                    } else if (maxdamage && damage == maxdamage) {
                        if (t.effective() == units[maxindex].effective()) {
                            if (t.initiative > units[maxindex].initiative) {
                                maxindex = i;
                                targ = t.id;
                            }
                        } else if (t.effective() > units[maxindex].effective()) {
                            maxindex = i;
                            targ = t.id;
                        }
                    }
                }
                u.target = maxindex;
                if (maxindex != -1) units[maxindex].selected = true;
            }
            std::vector<std::tuple<int, int>> initlist;
            for (int i = 0; i < (int) units.size(); i++) {
                initlist.emplace_back(units[i].initiative, i);
            }

            std::sort(initlist.begin(), initlist.end(), std::greater<>());
            int k = 0;
            for (auto[_, i] : initlist) {
                auto &u = units[i];
                if (u.units <= 0) continue;
                if (u.target < 0) continue;
                auto &t = units[u.target];

                int modifier = t.weak.count(u.damagetype) ? 2 : 1;
                modifier *= t.immune.count(u.damagetype) ? 0 : 1;

                int damage = u.units * u.damage * modifier;

                int b = t.units;
                t.units -= damage / t.hp;
                k+=b-t.units;
            }
            if(!k) break;

            units.erase(std::remove_if(units.begin(), units.end(), [](auto &x) { return x.units <= 0; }), units.end());

            for (auto &u : units) {
                u.target = -1;
                u.selected = false;
            }

        }

        if(!ans1) {
            for (auto &u : units) { ans1 += u.units; }
        } else if(std::count_if(units.begin(), units.end(), [](auto & u) {return u.isinfection;}) == 0) {
            ans2 = 0;
            for (auto &u : units) { ans2 += u.units; }
            found = true;
        }

        for(auto & u : inputunits) {
            if(!u.isinfection) u.damage++;
        }
    }
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
