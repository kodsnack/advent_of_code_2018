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

template<class T> bool amax(T & a, const T & b) { bool res = b>a; a = max(a,b); return res; }
template<class T> bool amin(T & a, const T & b) { bool res = b>a; a = min(a,b); return res; }

template<class T> T getinword () {
    T temp;
    cin >> temp;
    return temp;
}

struct guard {
    int id, tot_sleep;
    vector<int> sleep;
    guard(int i) : id(i), tot_sleep(0), sleep(vector<int> (60,0)) {}
    guard() : id(-1), tot_sleep(0), sleep(vector<int> (60,0)) {}
    friend bool operator< (const guard & a, const guard & b) {
        return a.id < b.id;
    }
};


int main() {
    priority_queue<pair<int, int>, vector<pair<int, int>>, greater<pair<int, int>>> q;
    map<int, guard> guards;
    int year, month, day, hour, minute;
    string line;
    scanf("[%d-%d-%d %d:%d]", &year, &month, &day, &hour, &minute);
    while (getline(cin, line)) {
        int id = -1;
        int ind = line.find('#');
        int indsp = line.find(' ',ind);
        if (ind != string::npos) {
            id = stoi(line.substr(ind+1, indsp-ind-1));
            guards.insert({id, {id}});
        }
        q.push({year*535680+month*44640+day*1440+hour*60+minute, id});
        scanf("[%d-%d-%d %d:%d]", &year, &month, &day, &hour, &minute);
    }

    while (!q.empty()) {
        pair<int, int> tmp = q.top(); q.pop();
        int id = tmp.second;
        while (q.top().second == -1 && !q.empty()) {
            tmp = q.top(); q.pop();
            int start = tmp.first;
            tmp = q.top(); q.pop();
            int end = tmp.first;
            
            guards[id].tot_sleep += end-start;
            for (int i=start%60; i < end%60; i++) {
                guards[id].sleep[i]++;
            }
        }
    }
    guard mx_guard;
    for (pair<int, guard> g : guards) {
        guard grd = g.second;
        if (mx_guard.tot_sleep < grd.tot_sleep) {
            mx_guard = grd;
        }
    }
    int mx_min=0, mx=0;
    for (int i=0; i < 60; i++) {
        if (amax(mx, mx_guard.sleep[i])) {
            mx_min = i;
        }
    }
    cout << mx_guard.id*mx_min << "\n";
}
