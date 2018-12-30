#include <iostream>
#include <vector>

int main() {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<std::vector<char>> input;
    input.emplace_back();
    {
        bool done = false;

        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c == '\n') {
                input.emplace_back();
            } else {
                input.back().push_back(c);
            }

        }
    }

    auto next = input;
    bool cycle_found = false;
    int i = 0;
    std::vector<int> memory;

    while(!cycle_found || i < 10) {
        int tl = 0, tt = 0;
        for(int y = 0; y < (int)input.size(); y++) {
            for(int x = 0; x < (int)input[y].size(); x++) {
                int t = 0, l = 0;
                for(int dy = -1; dy <= 1; dy++) {
                    for(int dx = -1; dx <= 1; dx++) {
                        if(dy == 0 && dx == 0) continue;
                        if(y+dy < 0 || x+dx < 0 || y+dy >= (int)input.size() || x+dx >= (int)input[y+dy].size()) continue;
                        if(input[y+dy][x+dx] == '#') l++;
                        if(input[y+dy][x+dx] == '|') t++;
                    }
                }

                if(input[y][x] == '.') {
                    if(t >= 3) { next[y][x] = '|'; tt++; }
                    else next[y][x] = '.';
                } else if(input[y][x] == '|') {
                    if(l >= 3) { next[y][x] = '#'; tl++; }
                    else { next[y][x] = '|'; tt++; }
                } else if(input[y][x] == '#') {
                    if(!(l && t)) next[y][x] = '.';
                    else { next[y][x] = '#'; tl++; }
                }
            }
        }
        input.swap(next);
        auto tans = tl*tt;

        for(auto k = (int)memory.size(); k > 0; k--) {
            if(memory[k] == tans) {
                int diff = i-k;
                if(k-diff >= 0 && memory[k-diff] == tans) {
                    int idx = (1000000000-i) % diff;
                    ans2 = memory[k-diff+idx-1];
                    cycle_found = true;
                }
            }
        }
        memory.push_back(tans);

        i++;
        if(i == 10) ans1 = tans;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
