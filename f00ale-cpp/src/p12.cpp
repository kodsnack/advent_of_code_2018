#include <iostream>
#include <vector>
#include <unordered_map>

int main() {
    int ans1 = 0;
    int64_t ans2 = 0;
    std::string pots;
    int offset = 0;
    std::unordered_map<std::string, char> rules;

    {
        bool done = false;
        bool have_initial = false;
        std::string tmp;
        char to = 0;

        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c == '.' || c == '#') {
                if(!have_initial) {
                    pots.push_back(c);
                } else {
                    if(tmp.length() < 5) tmp.push_back(c);
                    else to = c;
                }
            } else {
                if(c == '\n') {
                    if(!have_initial) {
                        have_initial = true;
                    } else if(tmp.length()==5){
                        rules.emplace(tmp, to);
                    }
                    tmp.clear();
                }
            }

        }
    }

    int last_count = 0;
    int diff = 0;
    int stable_count = 0;
    int gen;
    std::string next;

    for(gen = 0; gen < 20 || stable_count < 4; gen++) {
        auto first = pots.find('#');
        auto last = pots.rfind('#');
        pots = "...." + pots.substr(first, last-first+1) + "....";
        offset += first - 4 + 2;
        next.clear();
        next.reserve(pots.size());
        for(size_t i = 0; i+4<pots.length(); i++) {
            auto tmp = pots.substr(i, 5);
            auto it = rules.find(tmp);
            if(it != rules.end()) {
                next.push_back(it->second);
            }
        }
        pots.swap(next);
        int count = 0;
        for(auto it = pots.begin(); it != pots.end(); it++) {
            if(*it == '#') count += std::distance(pots.begin(), it)+offset;
        }
        if(gen+1 == 20) ans1 = count;
        if(count - last_count == diff) {
            stable_count++;
        } else {
            stable_count = 0;
        }
        diff = count - last_count;
        last_count = count;
    }

    ans2 = last_count + (50000000000-gen)*diff;

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
