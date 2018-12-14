#include <iostream>
#include <vector>
#include <array>
#include <map>
#include <climits>

int main() {
    std::string ans1;
    int ans2 = 0;

    struct data {
        int px, py, vx, vy;
    };
    std::vector<data> input;
    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        data tmp;
        int ndata = 0;
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
                    switch(ndata) {
                        case 0: tmp.px = num; break;
                        case 1: tmp.py = num; break;
                        case 2: tmp.vx = num; break;
                        case 3: tmp.vy = num; break;
                    }
                    ndata++;
                }
                if(c == '\n') {
                    if(ndata == 4) {
                        input.push_back(tmp);
                    }
                    ndata = 0;
                }
                have_num = false;
                num = 0;
                neg = false;
            }

        }
    }

    int dx = INT_MAX, dy = INT_MAX;
    bool shrinking = true;
    auto sky = input;
    while(shrinking) {
        int maxx = INT_MIN, maxy = INT_MIN, minx = INT_MAX, miny = INT_MAX;
        auto cpy = sky;
        for (auto &d :cpy) {
            d.px += d.vx;
            d.py += d.vy;
            if (d.px > maxx) maxx = d.px;
            if (d.py > maxy) maxy = d.py;
            if (d.px < minx) minx = d.px;
            if (d.py < miny) miny = d.py;

        }
        if(maxx-minx < dx && maxy-miny < dy) {
            shrinking = true;
            sky = cpy;
            dx = maxx-minx;
            dy = maxy-miny;
            ans2++;
        } else {
            shrinking = false;
        }
    }

    std::vector<std::vector<char>> output;
    for(int i = 0; i <= dy; i++) output.emplace_back(dx+1);

    int minx = INT_MAX, miny = INT_MAX;
    for(auto & d : sky) {
        if(d.px < minx) minx=d.px;
        if(d.py < miny) miny=d.py;
    }
    for(auto & d : sky) {
        output[d.py-miny][d.px-minx] = '#';
    }

    auto dumpoutput = [&output] {
        for (int i = 0; i < 64; i++) if (i % 8 == 0) std::cout << '|'; else std::cout << '-';
        std::cout << std::endl;
        for (auto &r : output) {
            for (auto &c : r) {
                std::cout << (c ? c : ' ');
            }
            std::cout << std::endl;
        }
    };
    // extract characters
    // assume characters 8 pixels wide, 10 pixels high

    using bitmap = std::array<int, 10>;
    std::map<bitmap, char> m {
            {{
                     0b11111000,
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b11111000,
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b11111000}, 'B'},
            {{
                     0b01111000,
                     0b10000100,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b10000100,
                     0b01111000}, 'C'},
            {{
                     0b11111100,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b11111000,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b10000000,
                     0b11111100}, 'E'},
            {{
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b11111100,
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b10000100,
                     0b10000100}, 'H'},
            {{
                     0b00011100,
                     0b00001000,
                     0b00001000,
                     0b00001000,
                     0b00001000,
                     0b00001000,
                     0b00001000,
                     0b10001000,
                     0b10001000,
                     0b01110000}, 'J'},
            {{
                     0b10000100,
                     0b10001000,
                     0b10010000,
                     0b10100000,
                     0b11000000,
                     0b11000000,
                     0b10100000,
                     0b10010000,
                     0b10001000,
                     0b10000100}, 'K'},
            {{
                     0b11111100,
                     0b00000100,
                     0b00000100,
                     0b00001000,
                     0b00010000,
                     0b00100000,
                     0b01000000,
                     0b10000000,
                     0b10000000,
                     0b11111100}, 'Z'},

    };

    bool decoded = true;
    for(int cx = 0; cx < dx; cx+=8) {
        bitmap ch = {0, };
        for(int y = 0; y < 10; y++) {
            int tmp = 0;
            for(int x = cx; x < cx+8; x++) {
                tmp <<= 1;
                if(x < (int)output[y].size()) {
                    tmp |= output[y][x] ? 1 : 0;
                }
            }
            ch[y] = tmp;
        }
        if(auto it = m.find(ch); it != m.end()) {
            ans1.push_back(it->second);
        } else {
            std::cout << "unable to decode (\"OCR\") char at position " << (cx/8) << std::endl;
            decoded = false;
            ans1.push_back('_');
        }
    }

    if(!decoded) dumpoutput();

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
