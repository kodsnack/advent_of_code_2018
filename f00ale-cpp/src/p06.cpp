#include <iostream>
#include <vector>
#include <tuple>

int main() {
    int ans1 = 0;
    int ans2 = 0;

    std::vector<std::tuple<int,int>> input;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        bool have_num1 = false;

        int num1 = 0, num2 = 0;
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
                    if(!have_num1) {
                        num1 = num;
                        have_num1 = true;
                    } else {
                        num2 = num;
                    }
                }
                if(c == '\n') {
                    if(have_num1) {
                        input.emplace_back(num1, num2);
                    }
                    num1 = num2 = 0;
                    have_num1 = false;
                }
                have_num = false;
                num = 0;
            }

        }
    }

    int minx = INT_MAX, miny = INT_MAX;
    int maxx = 0, maxy = 0;

    for(const auto & [x,y] : input) {
        if(x > maxy) maxx = x;
        if(y > maxy) maxy = y;
        if(x < minx) minx = x;
        if(y < miny) miny = y;
    }

    std::vector<int> areas(input.size());

    for(int x = minx; x <= maxx; x++) {
        for(int y = miny; y <= maxy; y++) {
            int md = INT_MAX;
            int mi = 0;
            bool tie = false;
            int tot_d = 0;
            for(int i = 0; i < input.size(); i++) {
                const auto [x0,y0] = input[i];
                int dx = abs(x-x0);
                int dy = abs(y-y0);
                if(dx+dy == md) tie = true;
                else if(dx+dy < md) {
                    md = dx+dy;
                    tie = false;
                    mi = i;
                }
                tot_d += dx + dy;
            }
            if(!tie) {
                areas[mi]++;
                if(x == 0 || y == 0 || x == maxx || y == maxy) {
                    // assume infinityness
                    areas[mi] = INT_MIN;
                }
            }
            if(tot_d < 10000) ans2++;
        }
    }

    ans1 = *std::max_element(areas.begin(), areas.end());

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
