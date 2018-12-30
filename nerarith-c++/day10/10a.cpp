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

template <typename T>
std::pair<T,T> operator+(const std::pair<T,T> & a,const std::pair<T,T> & b) {
    return {a.first+b.first,a.second+b.second};
}                                             

template <typename T>
std::pair<T,T> operator*(const int & s,const std::pair<T,T> & p) {
    return {s*p.first,s*p.second};
}                                             


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);
    
    vector<pair<int, int>> poss, vels;
    string s;
    while (getline(cin, s)) {
        pair<int, int> pos, vel;
        sscanf(s.c_str(), "position=<%d, %d> velocity=<%d, %d>", &pos.first, &pos.second, &vel.first, &vel.second);
        poss.push_back(pos);
        vels.push_back(vel);
    }

    int t=0;
    for (int mn=1e8, res=1e8-1; res < mn ; t++) {
        mn = res;
        int mnx=1e8, mxx=-1e8, mny=1e8, mxy=-1e8;
        for (int i=0; i < poss.size(); i++) {
            pair<int, int> curr = poss[i] + t*vels[i];
            amin(mnx, curr.first);  amax(mxx, curr.first);
            amin(mny, curr.second); amax(mxy, curr.second);
        }
        res = mxy-mny;
    }

    t-=2;
    int mnx=1e8, mxx=-1e8, mny=1e8, mxy=-1e8;
    for (int i=0; i < poss.size(); i++) {
        pair<int, int> curr = poss[i] + t*vels[i];
        amin(mnx, curr.first);  amax(mxx, curr.first);
        amin(mny, curr.second); amax(mxy, curr.second);
    }
    vector<string> mes (mxy-mny+1, string (mxx-mnx+1, '.'));
    for (int i=0; i < poss.size(); i++) {
        pair<int, int> curr = poss[i] + t*vels[i];
        mes[curr.second-mny][curr.first-mnx] = '#';
    }
    for (int i=0; i < mes.size(); i++) {
        cout << mes[i] << "\n";
    }
}
