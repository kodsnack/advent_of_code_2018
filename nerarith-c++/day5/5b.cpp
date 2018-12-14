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

int reduced_size (string s) {
    for (int i=0; i < s.size()-1; i++) {
        if (s[i] - 'a' == s[i+1] - 'A' || s[i] - 'A' == s[i+1] - 'a') {
            s.erase(i, 2);
            i = max(i-2, -1);
        }
    }
    return s.size();
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    string s;
    cin >> s;
    int mn_size = s.size();
    for (char c=0; c <= 'z'; c++) {
        string shortened (s.size(), 'z');
        auto it = copy_if(all(s), shortened.begin(), [c](char elem){return elem!=c && elem!=c-'a'+'A';});
        shortened.resize(it - shortened.begin());
        amin(mn_size, (int) reduced_size(shortened));
    }
    cout << mn_size << "\n";
}
