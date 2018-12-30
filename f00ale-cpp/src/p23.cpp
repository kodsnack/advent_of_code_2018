#include <iostream>
#include <vector>
#include <tuple>
#include <array>
#include <stdint.h>
#include <map>

template<typename T>
T myabs(T t) { return t < 0 ? -t : t; }

int main() {
    int ans1 = 0;
    int ans2 = 0;

    std::vector<std::tuple<int,int,int,int>> input;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        std::array<int,4> tmp;
        int ant = 0;
        bool neg = false;
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
                    if(neg) num *= -1;
                    tmp[ant] = num;
                    ant++;
                }
                if(c == '\n') {
                    if(ant == 4) {
                        input.emplace_back(tmp[0], tmp[1], tmp[2], tmp[3]);
                    }
                    ant = 0;
                }
                have_num = false;
                num = 0;
                neg = false;
            }

        }
    }

    // part 1

    auto [mx,my,mz,mr] = *std::max_element(input.begin(), input.end(),
            [](const auto & t1, const auto & t2) { return std::get<3>(t1) < std::get<3>(t2); });

    for (auto & nb : input) {
        auto [x,y,z,r] = nb;
        auto dx = myabs(mx-x);
        auto dy = myabs(my-y);
        auto dz = myabs(mz-z);
        if(dx+dy+dz <= mr) ans1++;
    }

    // part 2
    auto maxx = std::get<0>(*std::max_element(input.begin(), input.end(),
                                              [](auto & t1, auto & t2) { return std::get<0>(t1) < std::get<0>(t2); }));
    auto maxy = std::get<1>(*std::max_element(input.begin(), input.end(),
                                              [](auto & t1, auto & t2) { return std::get<1>(t1) < std::get<1>(t2); }));
    auto maxz = std::get<2>(*std::max_element(input.begin(), input.end(),
                                              [](auto & t1, auto & t2) { return std::get<2>(t1) < std::get<2>(t2); }));
    auto minx = std::get<0>(*std::min_element(input.begin(), input.end(),
                                              [](auto & t1, auto & t2) { return std::get<0>(t1) < std::get<0>(t2); }));
    auto miny = std::get<1>(*std::min_element(input.begin(), input.end(),
                                              [](auto & t1, auto & t2) { return std::get<1>(t1) < std::get<1>(t2); }));
    auto minz = std::get<2>(*std::min_element(input.begin(), input.end(),
                                              [](auto & t1, auto & t2) { return std::get<2>(t1) < std::get<2>(t2); }));

    auto maxside = std::max(myabs(maxx-(minx<0?minx:0)), std::max( myabs(maxy-(miny<0?miny:0)), myabs(maxz-(minz<0?minz:0))));

    decltype(maxside) side = 1;
    while(side < maxside) side *= 2;

    while(side > 1) {
        int maxcount = 0;
        int x0 = 0, y0 = 0, z0 = 0;
        for(auto x = minx; x <= maxx; x+=side) {
            for(auto y = miny; y <= maxy; y+=side) {
                for(auto z = minz; z <= maxz; z+=side) {
                    int cnt = 0;
                    for(const auto & bot : input) {
                        auto [bx,by,bz,br] = bot;
                        auto dist = myabs(x-bx)+myabs(y-by)+myabs(z-bz);
                        if((dist-br)/side <= 0) cnt++;
                    }
                    if(cnt >= maxcount) {
                        if(cnt > maxcount || myabs(x)+myabs(y)+myabs(z) < ans2) {
                            maxcount = cnt;
                            ans2 = myabs(x) + myabs(y) + myabs(z);
                            x0 = x;
                            y0 = y;
                            z0 = z;
                        }
                    }
                }
            }
        }
        minx = x0 - side;
        miny = y0 - side;
        minz = z0 - side;
        maxx = x0 + side;
        maxy = y0 + side;
        maxz = z0 + side;
        side /= 2;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
