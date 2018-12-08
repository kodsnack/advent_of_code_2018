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

int interpret (const vector<int> & nums, int & i) {
    int res = 0;
    int nchilds = nums[i];
    i++;
    PRINTLN("nchilds:  " << nchilds);
    vector<int> childs (nchilds);
    int ndata = nums[i];
    i++;
    for (int j=0; j < nchilds; j++) {
        childs[j] = interpret(nums, i);
    }
    if (nchilds == 0) {
        while (ndata--) {
            res += nums[i];
            i++;
        }
    }
    else {
        while (ndata--) {
            int data = nums[i];
            if (0 < data && data <= nchilds) {
                res += childs[data-1];
            }
            i++;
        }
    }
    return res;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    vector<int> nums;
    int num;
    while (cin >> num) {
        nums.push_back(num);
    }
    int i=0;
    cout << interpret(nums, i) << "\n";
}
