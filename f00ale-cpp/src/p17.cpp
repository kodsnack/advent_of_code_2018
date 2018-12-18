#include <iostream>
#include <vector>
#include <climits>

void dumpmap(const std::vector<std::vector<char>> & map) {
    for(auto const & n : map) {
        for (auto &c : n) std::cout << c;
        std::cout << std::endl;
    }
    std::cout << std::endl;
}

void fill(std::vector<std::vector<char>> & map, int x, int y) {
    if(y < 0 || y >= (int)map.size() || x < 0 || x >= (int)map[y].size()) return;
    if(map[y][x] == '.' || map[y][x] == '+') {
        if(map[y][x] == '.') map[y][x] = '|';
        if(y+1 < (int)map.size()) {
            if(map[y+1][x] == '.') {
                fill(map, x, y+1);
            }
            if(map[y+1][x] == '#' || map[y+1][x] == '~'){
                int edge[2] = {0,0};
                for(int i = 0; i < 2; i++) {
                    int dx = -1 + 2*i;
                    for(edge[i] = x+dx; edge[i] >= 0 && edge[i] < (int)map[y].size(); edge[i]+=dx) {
                        if(map[y][edge[i]] == '.') {
                            map[y][edge[i]] = '|';
                            if(map[y+1][edge[i]] == '.') {
                                fill(map, edge[i], y+1);
                                if(map[y+1][edge[i]] != '~') break;
                            }
                        }
                        else break;
                    }
                }
                if(edge[0] >= 0 && edge[1] < (int)map[y].size() && map[y][edge[0]] == '#' && map[y][edge[1]] == '#') {
                    for(int tx = edge[0]+1; tx < edge[1]; tx++) {
                        map[y][tx] = '~';
                    }
                }
            }
        }
    }
}


int main() {
    int ans1 = 0;
    int ans2 = 0;

    struct node {
        int xmin, xmax, ymin, ymax;
        node(int _x1, int _x2, int _y1, int _y2) : xmin{_x1}, xmax{_x2}, ymin{_y1}, ymax{_y2} {}
    };

    std::vector<node> input;
    int minx = INT_MAX, maxx = INT_MIN;
    int miny = INT_MAX, maxy = INT_MIN;
    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        bool is_x = false, is_y = false;
        int x = 0, y = 0, last = 0;
        bool have_x = false, have_y = false;
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
            } else if(c == 'x') {
                is_x = true;
                is_y = false;
            } else if(c == 'y') {
                is_y = true;
                is_x = false;
            } else {
                if(have_num) {
                    if(!have_x && is_x) {
                        x = num;
                        have_x = true;
                    } else if(!have_y && is_y) {
                        y = num;
                        have_y = true;
                    } else if(have_x && have_y) {
                        last = num;
                    }
                }

                if(c == '\n') {
                    if(have_x && have_y) {
                        if(is_x) {
                            input.emplace_back(x,last,y,y);
                            if(x < minx) minx = x;
                            if(last > maxx) maxx = last;

                            if(y < miny) miny = y;
                            if(y > maxy) maxy = y;
                        }
                        else {
                            input.emplace_back(x,x,y,last);
                            if(x < minx) minx = x;
                            if(x > maxx) maxx = x;

                            if(y < miny) miny = y;
                            if(last > maxy) maxy = last;
                        }
                    }

                    have_x = have_y = is_x = is_y = false;

                    x = y = last = 0;
                }
                have_num = false;
                num = 0;
            }

        }
    }

    std::vector<std::vector<char>> map;

    for(auto const & n : input) {
        while((int)map.size() <= maxy) {
            map.emplace_back();
            while((int)map.back().size() <= maxx - minx + 2) map.back().push_back('.');
            if(map.size() == 1) map.back()[500-minx+1] = '+';
        }
        for(auto y = n.ymin; y <= n.ymax; y++) {
            for(auto x = n.xmin; x <= n.xmax; x++)
                map[y][x-minx+1] = '#';
        }
    }

    fill(map, 500-minx+1, 0);

    //dumpmap(map);

    bool first_seen = false;
    for(auto const & n : map) {
        for (auto &c : n) {
            if(c == '~') { ans1++; ans2++; }
            if(c == '|') ans1++;
            if(c == '#') first_seen = true;
        }
        if(!first_seen) ans1 = ans2 = 0;
    }


    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
