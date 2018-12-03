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
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    vector<int> changes;
    int num;
    while (cin >> num) {
        changes.push_back(num);
    }
        
    int curr_freq=0;
    set<int> prev_freq;
    prev_freq.insert(curr_freq);
    for (int i=0; true; i=(i+1)%changes.size()) {
        curr_freq+=changes[i];
        if (prev_freq.find(curr_freq) != prev_freq.end()) {
            cout << curr_freq << "\n";
            break;
        }
        prev_freq.insert(curr_freq);
    }
}
