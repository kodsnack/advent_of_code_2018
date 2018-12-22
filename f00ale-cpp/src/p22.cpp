#include <iostream>
#include <vector>
#include <tuple>
#include <deque>
#include <climits>
#include <algorithm>

int main() {
    int ans1 = 0;
    int ans2 = 0;

    int depth = 0, tx = 0, ty = 0;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        bool have_depth = false, have_x = false;
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
                    if(!have_depth) {
                        depth = num;
                        have_depth = true;
                    } else if(!have_x) {
                        tx = num;
                        have_x = true;
                    } else {
                        ty = num;
                    }
                }
                if(c == '\n') {

                }
                have_num = false;
                num = 0;
            }

        }
    }

    std::vector<std::vector<char>> map;
    std::vector<std::vector<int>> erosion;

    for(int y = 0; y <= 4*ty; y++) {
        map.emplace_back();
        erosion.emplace_back();
        for(int x = 0; x <= 4*tx; x++) {
            int idx = 0;
            if(!y) idx = x*16807;
            else if(!x) idx = y*48271;
            else idx = erosion[y][x-1]*erosion[y-1][x];
            if(y == ty && x == tx) idx = 0;
            erosion[y].push_back((idx+depth) % 20183);
            switch(((idx+depth)%20183)%3) {
                case 0: map[y].push_back('.'); break;
                case 1: map[y].push_back('='); break;
                case 2: map[y].push_back('|'); break;
            }
            if(y <= ty && x <= tx) ans1 += ((idx+depth)%20183)%3;
        }
    }

    std::vector<std::vector<std::vector<int>>> cost;
    for(auto & r: map){
        cost.emplace_back();
        for(auto & x: r) {
            (void)x;
            cost.back().emplace_back();
            for(int i = 0; i < 3; i++)
                cost.back().back().push_back(INT_MAX);
        }
    }
    using step = std::tuple<int,int,int,int>; // x,y,cost,equip: 1 = torch 2 = climb
    std::deque<step> q;
    q.emplace_back(0,0,0,1);
    int mincost = INT_MAX;
    while(!q.empty()) {
        const auto [x,y,c,e] = q.front();
        q.pop_front();
        if(x < 0 || y < 0 || y >= (int)cost.size() || x >= (int)cost[y].size()) continue;

        if(c > mincost) continue; // target found at lower cost. Abort this track.

        if((map[y][x] == '.' && e == 0) || (map[y][x] == '=' && e == 1) || (map[y][x] == '|' && e == 2)) continue;

        for(int ne = 0; ne < 3; ne++) {
            if((map[y][x] == '.' && ne == 0) || (map[y][x] == '=' && ne == 1) || (map[y][x] == '|' && ne == 2)) continue;
            int nc = c + (e == ne ? 0 : 7);
            if(nc < cost[y][x][ne]) cost[y][x][ne] = nc;
            else continue;
            if(x == tx && y == ty) {
                if(c < mincost) mincost = c;
            }

            q.emplace_back(x, y - 1, nc + 1, ne);
            q.emplace_back(x - 1, y, nc + 1, ne);
            q.emplace_back(x + 1, y, nc + 1, ne);
            q.emplace_back(x, y + 1, nc + 1, ne);
        }
    }

    ans2 = cost[ty][tx][1];
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
