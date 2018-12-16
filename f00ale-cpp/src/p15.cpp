#include <iostream>
#include <vector>
#include <set>
#include <deque>
#include <tuple>
#include <climits>
#include <fstream>

struct node{
    char c;
    bool used;
    int hp;
    node(char _c, int _h) : c{_c}, used{false}, hp{_h} {}
};

struct point {
    int x;
    int y;
    point(int _x, int _y) : x{_x}, y{_y} {}
};
bool operator<(const point & p1, const point & p2) { return std::tie(p1.y, p1.x) < std::tie(p2.y, p2.x); }
struct steppoint : public point {
    int s;
    int sdx;
    int sdy;
    steppoint(int _s, int _x, int _y, int _sdx, int _sdy) : point{_x, _y}, s{_s}, sdx{_sdx}, sdy{_sdy} {}
};

void dumpmap(const std::vector<std::vector<node>> & map) {
    for (const auto &row : map) {
        for (auto &col : row) {
            std::cout << col.c;
        }
        std::cout << " ";
        for (auto &col : row) {
            if (col.c == 'G' || col.c == 'E') std::cout << " " << col.c << '(' << col.hp << ')';
        }

        std::cout << std::endl;

    }
}

auto p15_solve(std::vector<std::vector<node>> map /* take copy */, int elves, int goblins, int elfpower, bool allowelfdeath, bool debug = false) {
    int answer = 0;
    int turn = 0, doneturn = 0;

    while (elves && goblins) {
        for (auto rowit = map.begin(); rowit != map.end(); rowit++) {
            int rowy = std::distance(map.begin(), rowit);

            for (auto colit = rowit->begin(); colit != rowit->end(); colit++) {
                auto &n = *colit;
                if (n.c != 'E' && n.c != 'G') continue;
                if (!doneturn && (goblins == 0 || elves == 0)) doneturn = turn;

                if (n.used) continue;
                n.used = true;

                int x = std::distance(rowit->begin(), colit);
                int y = rowy;
                const char enemy = (n.c == 'E' ? 'G' : 'E');

                if (map[y - 1][x].c != enemy && map[y][x - 1].c != enemy && map[y][x + 1].c != enemy &&
                    map[y + 1][x].c != enemy) {
                    // 1. find all reachables at minimum distance
                    // 2. select the one with least "reading order"
                    // 3. step towards it, minimizing own reading order

                    // find min dist
                    std::set<point> found;
                    {
                        std::set<point> visited;
                        std::deque<point> tocheck;
                        visited.emplace(x, y);
                        tocheck.emplace_back(x, y);
                        int step = -1;
                        while (found.empty() && !tocheck.empty()) {
                            step++;
                            std::deque<point> next;
                            for (auto &p : tocheck) {
                                for (int dy = -1; dy <= 1; dy++) {
                                    for (int dx = -1; dx <= 1; dx++) {
                                        if (dx && dy) continue;
                                        if (map[p.y + dy][p.x + dx].c == enemy) {
                                            found.emplace(p.x, p.y);
                                        } else if (map[p.y + dy][p.x + dx].c == '.' &&
                                                   visited.find({p.x + dx, p.y + dy}) == visited.end()) {
                                            visited.emplace(p.x + dx, p.y + dy);
                                            next.emplace_back(p.x + dx, p.y + dy);
                                        }
                                    }
                                }
                            }
                            tocheck.swap(next);
                        }

                        if (debug) {
                            std::cout << x << "," << y << ": " << step << std::endl;
                            for (auto &p : found) {
                                std::cout << "  " << p.x << "," << p.y << std::endl;
                            }
                        }
                    }
                    if (!found.empty()) {
                        auto target = *found.begin();

                        std::set<point> visited;
                        std::deque<steppoint> tocheck;
                        bool found = false;
                        int sdx = 0, sdy = 0;
                        int step = 0;
                        visited.emplace(x, y);
                        visited.emplace(x, y - 1);
                        visited.emplace(x - 1, y);
                        visited.emplace(x + 1, y);
                        visited.emplace(x, y + 1);
                        tocheck.emplace_back(1, x, y - 1, 0, -1);
                        tocheck.emplace_back(1, x - 1, y, -1, 0);
                        tocheck.emplace_back(1, x + 1, y, +1, 0);
                        tocheck.emplace_back(1, x, y + 1, 0, +1);

                        while (!found && !tocheck.empty()) {
                            auto current = tocheck.front();
                            tocheck.pop_front();

                            if (current.y == target.y && current.x == target.x) {
                                found = true;
                                step = current.s;
                                sdx = current.sdx;
                                sdy = current.sdy;
                            } else if (map[current.y][current.x].c == '.') {
                                for (int dy = -1; dy <= 1; dy++) {
                                    for (int dx = -1; dx <= 1; dx++) {
                                        if (dx && dy) continue;
                                        if (visited.find({current.x + dx, current.y + dy}) == visited.end()) {
                                            visited.emplace(current.x + dx, current.y + dy);
                                            tocheck.emplace_back(current.s + 1, current.x + dx, current.y + dy,
                                                                 current.sdx,
                                                                 current.sdy);
                                        }
                                    }
                                }
                            }

                        }

                        if (found && step) {
                            map[y + sdy][x + sdx] = map[y][x];
                            map[y][x] = node('.', 0);
                            x += sdx;
                            y += sdy;
                        }

                    }
                }

                //if(enemy == map[y-1][x].c || enemy == map[y][x-1].c || enemy == map[y][x+1].c || enemy == map[y+1][x].c) {
                // attack
                // find neighbour with lowest hp
                int lowdx = 0, lowdy = 0;
                int lowest = INT_MAX;
                for (int dy = -1; dy <= 1; dy++) {
                    for (int dx = -1; dx <= 1; dx++) {
                        if (dx && dy) continue;
                        auto &n = map[y + dy][x + dx];
                        if (n.c == enemy && n.hp < lowest) {
                            lowest = n.hp;
                            lowdx = dx;
                            lowdy = dy;
                        }
                    }
                }

                if (lowest < INT_MAX) {
                    auto &en = map[y + lowdy][x + lowdx];
                    auto &n = map[y][x];
                    if(n.c == 'E')
                        en.hp -= elfpower;
                    else
                        en.hp -= 3;
                    if (en.hp <= 0) {
                        if (en.c == 'E') {
                            if(allowelfdeath) {
                                elves--;
                            } else {
                                elves = 0;
                            }
                        }
                        else goblins--;
                        en.c = '.';
                        en.hp = 0;
                    }
                }
                //}
            }
        }

        for (auto &r : map) for (auto &c : r) c.used = false;
        turn++;
        if (debug) {
            std::cout << turn << std::endl;
            dumpmap(map);
        }
        //return std::tuple<int,int>(0,0);
    }

    if(!doneturn) doneturn = turn;
    answer = 0;
    for(auto & r : map) for(auto & c : r) answer += c.hp;
    if(debug) std::cout << answer << " " << doneturn << " " << elfpower << std::endl;
    answer *= doneturn;

    if(!elves && !allowelfdeath) answer = 0;

    return answer;
}

auto p15(std::istream & is, bool debug = false) {
    std::vector<std::vector<node>> inputmap;
    inputmap.emplace_back();

    int elves = 0, goblins = 0;
    {
        bool done = false;

        while (!done) {
            char c;
            is.get(c);
            if (!is.good()) {
                done = true;
                c = '\n';
            }

            if(c == '\n') {
                inputmap.emplace_back();
            } else {
                int hp = 0;
                if(c == 'G' || c == 'E') hp = 200;
                if(c == 'G') goblins++;
                if(c == 'E') elves++;
                inputmap.back().emplace_back(c, hp);
            }
        }
    }

    while(!inputmap.empty() && inputmap.back().empty()) inputmap.pop_back();

    if(debug) dumpmap(inputmap);
    int ans1 = p15_solve(inputmap, elves, goblins, 3, true, debug);
    int ans2 = 0;
    int elfpower = 3;
    while(!ans2) {
        ans2 = p15_solve(inputmap, elves, goblins, ++elfpower, false, debug);
    }
    return std::tuple(ans1,ans2);
}

int main() {
    auto [a1,a2] = p15(std::cin);
    std::cout << a1 << std::endl << a2 << std::endl;
}
