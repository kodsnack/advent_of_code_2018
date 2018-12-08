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

    priority_queue<int, vector<int>, greater<int>> q;
    for (int i=0; i < 26; i++) {
        if (parents[i] == 0) {
            q.push(i);
            parents[i] = 1;
        }
    }

    vector<bool> done (26, false);
    while (!q.empty()) {
        int curr = q.top(); q.pop();
        parents[curr]--;
        if (parents[curr] == 0 && !done[curr]) {
            done[curr] = true;
            cout << char('A' + curr);
            for (int c : children[curr]) {
                q.push(c);
            }
        }
    }
    cout << "\n";
}
