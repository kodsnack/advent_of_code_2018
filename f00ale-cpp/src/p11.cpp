#include <iostream>
#include <vector>
#include <climits>

int main() {
    std::string ans1;
    std::string ans2;
    int input = 0;
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

            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                have_num = true;
            } else {
                if(have_num) input = num;
                have_num = false;
                num = 0;
            }

        }
    }

    std::vector<std::vector<int>> grid(300);
    for(auto & col : grid) col.resize(300);

    auto calc = [](int x, int y, int serial) {
        auto rid = x + 10;
        int pl = rid * y;
        int s3 = pl + serial;
        int s4 = rid * s3;
        int h = (s4 % 1000) / 100;
        return h-5;
    };

    for(int r = 0; r < 300; r++) {
        for(int c = 0; c < 300; c++) {
            grid[r][c] = calc(c+1,r+1,input);
        }
    }

    int rmax = 0, cmax = 0, smax = 0;
    int max = INT_MIN;

    {
        int s = 3;
        for (int r = 0; r < 300 - s; r++) {
            for (int c = 0; c < 300 - s; c++) {
                int tmp = 0;
                for (int dr = 0; dr < s; dr++) {
                    for (int dc = 0; dc < s; dc++) {
                        tmp += grid[r + dr][c + dc];
                    }
                }
                if (tmp > max) {
                    max = tmp;
                    rmax = r;
                    cmax = c;
                }
            }
        }
        ans1 = std::to_string(cmax+1) + "," + std::to_string(rmax+1);
    }

    auto rowsum = grid;
    auto colsum = grid;

    for(int r = 0; r < 300; r++) {
        for(int c = 1; c < 300; c++) {
            rowsum[r][c] = rowsum[r][c-1] + grid[r][c];
        }
    }
    for(int c = 0; c < 300; c++) {
        for(int r = 1; r < 300; r++) {
            colsum[r][c] = colsum[r-1][c] + grid[r][c];
        }
    }

    max = INT_MIN;
    rmax = cmax = smax = 0;

    for (int r = 0; r < 300; r++) {
        for (int c = 0; c < 300; c++) {
            int tmp = grid[r][c];
            for(int s = 1; s+c < 300 && s+r < 300; s++) {
                tmp += rowsum[r+s][c+s-1] - (c>0 ? rowsum[r+s][c-1] : 0);
                tmp += colsum[r+s-1][c+s] - (r>0 ? colsum[r-1][c+s] : 0);
                tmp += grid[r+s][c+s];
                if (tmp >= max) {
                    max = tmp;
                    rmax = r;
                    cmax = c;
                    smax = s;
                    ans2 = std::to_string(cmax+1) + "," + std::to_string(rmax+1) + "," + std::to_string(smax+1);
                }
            }
        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
