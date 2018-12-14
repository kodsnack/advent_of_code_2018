#include <iostream>
#include <vector>
#include <tuple>
#include <algorithm>

struct cart {
    int x, y, dx, dy, state;
    bool crashed = false;
    cart(int _x, int _y, int _dx, int _dy, int _s) : x{_x}, y{_y}, dx{_dx}, dy{_dy}, state{_s} {}
};

bool operator<(const cart & c1, const cart & c2) {
    return std::tie(c1.y, c1.x) < std::tie(c2.y, c2.x);
}

int main() {
    std::string ans1;
    std::string ans2;

    std::vector<std::vector<char>> track;
    std::vector<cart> carts;
    {
        bool done = false;
        track.emplace_back();
        int x = 0, y = 0;
        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c == '\n') {
                track.emplace_back();
                y++;
                x=0;
            } else {
                if(c == 'v' || c == '^') {
                    carts.emplace_back(x,y,0,(c == 'v' ? 1 : -1),0);
                    c = '|';
                }
                else if(c == '>' || c == '<') {
                    carts.emplace_back(x,y,(c == '>' ? 1 : -1),0,0);
                    c = '-';
                }
                x++;
                track.back().push_back(c);
            }
        }
    }

    while(carts.size() > 1) {
        std::sort(carts.begin(), carts.end());
        for (auto &c: carts) {
            if(c.crashed) continue;
            c.x += c.dx;
            c.y += c.dy;
            switch(track[c.y][c.x]) {
                case '+':
                {
                    switch(c.state) {
                        case 0:
                            if(c.dx) { c.dy = -c.dx; c.dx = 0; }
                            else { c.dx = c.dy; c.dy = 0; }
                            break;
                        case 2:
                            if(c.dx) { c.dy = c.dx; c.dx = 0; }
                            else { c.dx = -c.dy; c.dy = 0; }
                            break;
                    }
                    c.state++;
                    if(c.state > 2) c.state = 0;
                }
                break;
                case '/':
                    if(c.dx) { c.dy = -c.dx; c.dx = 0; }
                    else { c.dx = -c.dy; c.dy = 0; }
                    break;
                case '\\':
                    if(c.dx) { c.dy = c.dx; c.dx = 0; }
                    else { c.dx = c.dy; c.dy = 0; }
                    break;
            }

            for(auto & c2 : carts) {
                if(c2.crashed) continue;
                if(&c == &c2) continue;
                if(c.x == c2.x && c.y == c2.y) {
                    if(ans1.empty()) {
                        ans1 = std::to_string(c.x) + "," + std::to_string(c.y);
                    }
                    c.crashed = true;
                    c2.crashed = true;
                }
            }
        }
        carts.erase(std::remove_if(carts.begin(), carts.end(), [](auto & c) {return c.crashed;}),
                carts.end());
    }

    ans2 = std::to_string(carts[0].x) + "," + std::to_string(carts[0].y);

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
