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

template<typename It>
It step_forward (It beg, It end, It it, int n) {
    for (int i=0; i < n; i++) {
        if (it == end)
            it = beg;
        it++;
    }
    return it;
}

template<typename It>
It step_backwards (It beg, It end, It it, int n) {
    for (int i=0; i < n; i++) {
        if (it == beg)
            it = end;
        it--;
    }
    return it;
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    int nplayers, nmarbles;
    scanf("%d players; last marble is worth %d points", & nplayers, & nmarbles);
    nmarbles *= 100;

    vector<long long> score (nplayers, 0);
    list<int> marbles = {0};
    list<int>::iterator curr = marbles.begin();
    for (int i=1; i <= nmarbles; i++) {
        if (i % 23 != 0) {
            curr = step_forward(all(marbles), curr, 2);
            curr = marbles.insert(curr, i);
        }
        else {
            curr = step_backwards(all(marbles), curr, 7);
            score[(i-1)%nplayers] += i + *curr;
            curr = marbles.erase(curr);
        }
    }

    long long mx=0;
    for (int i=0; i < nplayers; i++) {
        amax(mx, score[i]);
    }
    cout << mx << "\n";
}
