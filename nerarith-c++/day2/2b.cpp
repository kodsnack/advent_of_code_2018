#include <bits/stdc++.h>
using namespace std;

#if DEBUG
#include "prettyprint.hpp"
#define PRINTLN(x) \
    cerr << x << "\n"
#define PRINTSP(x) \
    cerr << x << " "
#else
#define PRINTLN(x)
#define PRINTSP(x)
#endif


int main() {
    string id;
    vector<set<string>> possible_commons;
    while (cin >> id) {
        while (possible_commons.size() < id.size()) {
            set<string> empty;
            possible_commons.push_back(empty);
        }
        PRINTLN("id:               " << id);
        for (int i=0; i < id.size(); i++) {
            string possible_common = id;
            possible_common.erase(i,1);
            PRINTLN("possible_common:  " << possible_common);
            if (possible_commons[i].find(possible_common) != possible_commons[i].end()) {
                cout << possible_common << "\n";
                return 0;
            }
            else {
                possible_commons[i].insert(possible_common);
            }
        }
    }
}
