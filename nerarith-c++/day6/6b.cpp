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

template<class T> bool amax(T & a, const T & b) { bool res=b>a; a = max(a,b); return res;}
template<class T> bool amin(T & a, const T & b) { bool res=b<a; a = min(a,b); return res;}

template<class T> T getinword () {
    T temp;
    cin >> temp;
    return temp;
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    vector<pair<int, int>> coordinates;
    int x, y, mnx=1e6, mxx=0, mny=1e6, mxy=0;
    char gar;
    while (cin >> x >> gar >> y) {
        coordinates.push_back({x, y});
        amin(mnx, x); amax(mxx, x);
        amin(mny, y); amax(mxy, y);
    }
    
    vector<bool> infinit (coordinates.size(), false);
    vector<int> area (coordinates.size(), 0);
    int res=0;
    for (int i=mnx; i <= mxx; i++) {
        for (int j=mny; j <= mxy; j++) {
            int d_tot = 0;
            for (int c=0; c < coordinates.size(); c++) {
                d_tot += abs(i-coordinates[c].first)+abs(j-coordinates[c].second);
            }
            if (d_tot < 10000) {
                res++;
            }
        }
    }

    cout << res << "\n";
}
