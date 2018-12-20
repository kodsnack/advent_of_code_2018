#include <iostream>
#include <vector>
#include <map>
#include <tuple>
#include <stack>

int main() {
    int ans1 = 0;
    int ans2 = 0;
    std::string input;
    input.reserve(20*1024);
    {
        bool done = false;

        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }
            input.push_back(c);
        }
    }

    using room = std::tuple<int,int>;
    using node = std::tuple<int,int,int>;
    std::map<room, int> steps;
    std::stack<node> st;
    int x = 0, y = 0;
    int s = 0;
    steps[room(x,y)] = 0;
    for(auto c : input) {
        switch(c) {
            case 'N':
                s++;
                y++;
                break;
            case 'S':
                s++;
                y--;
                break;
            case 'E':
                s++;
                x++;
                break;
            case 'W':
                s++;
                x--;
                break;
            case '(':
                st.emplace(s,x,y);
                break;
            case '|':
            case ')':
                std::tie(s,x,y) = st.top();
                if (c == ')') {
                    st.pop();
                }
                break;
            default:
                break;
        }
        if(steps.find(room(x,y)) == steps.end()) steps[room(x,y)] = s;
    }

    for(auto & n : steps) {
        if(n.second > ans1) ans1 = n.second;
        if(n.second >= 1000) ans2++;
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
