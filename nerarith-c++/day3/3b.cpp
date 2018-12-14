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
    
    vector<vector<int>> fab (2000, vector<int> (2000, 0));
    vector<bool> overlaps (1, true);

    int id=0, prev_id=-1, left, top, width, height;
    scanf("#%d @ %d,%d: %dx%d\n", &id, &left, &top, &width, &height);
    while (id != prev_id) {
        overlaps.push_back(false);
        for (int i=0; i < width; i++) {
            for (int j=0; j < height; j++) {
                if (fab[left+i][top+j] == 0) {
                    fab[left+i][top+j] = id;
                }
                else {
                    overlaps[id] = true;
                    overlaps[fab[left+i][top+j]] = true;
                }
            }
        }
        prev_id=id;
        scanf("#%d @ %d,%d: %dx%d\n", &id, &left, &top, &width, &height);
    }
    
    int res = 0;
    for (int i=1; i < overlaps.size(); i++) {
        if (!overlaps[i]) {
            cout << i << "\n";
        }
    }
}
