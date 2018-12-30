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

#define all(x) x.begin(), x.end()
#define rall(x) x.rbegin(), x.rend()

template<class T> void amax(T & a, const T & b) { a = max(a,b); }
template<class T> void amin(T & a, const T & b) { a = min(a,b); }

template<class T> T getinword () {
    T temp;
    cin >> temp;
    return temp;
}


int main() {
    vector<vector<int>> children (26);
    vector<int> parents (26, 0);
    string s;
    while (getline(cin, s)){
        char from, to;
        sscanf(s.c_str(), "Step %s must be finished before step %s can begin\n", &from, &to);
        children[from-'A'].push_back(to-'A');
        parents[to-'A']++;
    }

    priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> q;
    priority_queue<int, vector<int>, greater<int>> to_begin;
    int avail = 5;
    for (int i=0; i < 26; i++) {
        if (parents[i] == 0) {
            if (avail > 0) {
                q.push({61+i, i});
                avail--;
                parents[i] = 1;
            }
        }
    }

    vector<bool> done (26, false);
    int lasttime;
    while (!q.empty()) {
        pair<int, int> curr = q.top(); q.pop();
        avail++;
        parents[curr.second]--;
        if (parents[curr.second] == 0 && !done[curr.second]) {
            done[curr.second] = true;
            for (int c : children[curr.second]) {
                to_begin.push(c);
            }
        }
        while (avail > 0 && !to_begin.empty()) {
            int to_push = to_begin.top(); to_begin.pop();
            q.push({curr.first+61+to_push, to_push});
        }
        lasttime = curr.first;
    }
    cout << lasttime << "\n";
}
