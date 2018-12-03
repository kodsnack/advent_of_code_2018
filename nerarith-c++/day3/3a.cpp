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
    
    vector<vector<int>> fab (1000, vector<int> (1000, 0));

    int gar=0, prev_gar=-1, left, top, width, height;
    scanf("#%d @ %d,%d: %dx%d\n", &gar, &left, &top, &width, &height);
    while (gar != prev_gar) {
        for (int i=0; i < width; i++) {
            for (int j=0; j < height; j++) {
                fab[left+i][top+j]++;
            }
        }
        prev_gar=gar;
        scanf("#%d @ %d,%d: %dx%d\n", &gar, &left, &top, &width, &height);
    }
    
    int res = 0;
    for (int i=0; i < fab.size(); i++) {
        for (int j=0; j < fab.size(); j++) {
            if (fab[i][j] >= 2) {
                res++;
            }
        }
    }
    cout << res << "\n";
}
