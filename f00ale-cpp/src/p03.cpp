#include <iostream>
#include <vector>
#include <array>
int main() {
    int ans1 = 0;
    int ans2 = 0;

    std::vector<std::array<int, 5>> claims;

    {
        bool done = false;

        int num = 0;
        std::array<int,5> vals;
        int i = 0;
        bool have_num = false;

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
            } else if(have_num) {
                have_num = false;
                vals[i++] = num;
                if(c == '\n') {
                    claims.emplace_back(vals);
                    i = 0;
                }
                num = 0;
            }

        }
    }

    std::vector<std::vector<int>> fabric;

    for(const auto & c : claims) {
        for (int y = c[1]; y < c[1] + c[3]; y++) {
            fabric.reserve(c[1] + c[3] + 1);
            while (y >= (int)fabric.size()) fabric.emplace_back();
            for (int x = c[2]; x < c[2] + c[4]; x++) {
                fabric[y].reserve(c[2] + c[4] + 1);
                while (x >= (int)fabric[y].size()) fabric[y].emplace_back(0);
                fabric[y][x]++;
            }
        }
    }

    for(const auto & row : fabric) {
        for(auto c : row) {
            if(c>1) ans1++;
        }
    }

    for(const auto & c : claims) {
        bool found = true;
        for(int y = c[1]; y < c[1]+c[3]; y++) {
            for(int x = c[2]; x < c[2]+c[4]; x++) {
                if(fabric[y][x] > 1) {found = false;}
            }

        }
        if(found) ans2 = c[0];
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
