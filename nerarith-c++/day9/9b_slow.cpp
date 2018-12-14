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
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    long long nplayers, nmarbles;
    scanf("%lld players; last marble is worth %lld points", & nplayers, & nmarbles);
    nmarbles *= 100;

    vector<long long> score (nplayers, 0);
    vector<long long> marbles = {0};
    long long curr=-1;
    for (long long i=1; i <= nmarbles; i++) {
        if (i % 23 != 0) {
            if (curr == marbles.size()-2) {
                curr = marbles.size();
            }
            else {
                curr = (curr + 2) % marbles.size();
            }
            marbles.insert(marbles.begin()+curr, i);
        }
        else {
            curr = (curr - 7 + marbles.size()) % marbles.size();
            score[(i-1)%nplayers] += i + marbles[curr];
            marbles.erase(marbles.begin()+curr);
        }
    }

    long long mx=0;
    for (long long i=0; i < nplayers; i++) {
        amax(mx, score[i]);
    }
    cout << mx << "\n";
}
