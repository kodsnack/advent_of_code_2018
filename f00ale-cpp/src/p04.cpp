#include <iostream>
#include <vector>
#include <array>
#include <tuple>
#include <algorithm>
#include <map>
struct data {
    int year, month, day, hour, minute;
    int guard;
    bool f, w;
};
bool operator<(const data & d1, const data & d2) {
    return std::tie(d1.year, d1.month, d1.day, d1.hour, d1.minute) <
            std::tie(d2.year, d2.month, d2.day, d2.hour, d2.minute);

}
int main() {
    int ans1 = 0;
    int ans2 = 0;

    std::vector<data> v;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::array<int, 5> nums;
        int n = 0;
        bool have_a = false;
        bool wakes = false, falls = false;

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
                if(!have_a) {
                    if(c == 'w') {
                        wakes = true;
                        have_a = true;
                    } else if (c == 'f') {
                        falls = true;
                        have_a = true;
                    } else if (c == 'G') {
                        have_a = true;
                    }
                }
                if(have_num && n <5) {
                    nums[n++] = num;
                    num = 0;
                }
                if(c == '\n') {
                    if(n) {
                        v.push_back({nums[0], nums[1], nums[2], nums[3], nums[4], num, falls, wakes});
                    }
                    n = 0;
                    wakes = falls = false;
                    have_a = false;
                    num = 0;
                }
                have_num = false;
            }

        }
    }

    std::sort(v.begin(), v.end());

    std::map<int, std::array<int, 60>> sleeptime;
    std::map<int, int> totalsleep;

    {
        int guard = 0;
        int fell = 0;

        for (auto &d : v) {
            if (d.f) {
                fell = d.minute;
            } else if (d.w) {
                for (int i = fell; i < d.minute; i++)
                    sleeptime[guard][i]++;
                totalsleep[guard] += d.minute - fell;
            } else {
                guard = d.guard;
            }
        }
    }

    {
        int guard = 0;
        int maxsleep = 0;
        for (auto &p : totalsleep) {
            if (p.second > maxsleep) {
                guard = p.first;
                maxsleep = p.second;
            }
        }

        int maxminute, maxnights = 0;
        for (int i = 0; i < 60; i++) {
            if (sleeptime[guard][i] > maxnights) {
                maxnights = sleeptime[guard][i];
                maxminute = i;
            }
        }

        ans1 = maxminute * guard;
    }


    {
        int guard = 0;
        int minute, maxnights = 0;
        for (auto &p : sleeptime) {
            for (int i = 0; i < 60; i++) {
                if (p.second[i] > maxnights) {
                    maxnights = p.second[i];
                    minute = i;
                    guard = p.first;
                }
            }
        }
        ans2 = minute * guard;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
