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
template<class T> bool amin(T & a, const T & b) { bool res = b<a; a = min(a,b); return res; }

template<class T> T getinword () {
    T temp;
    cin >> temp;
    return temp;
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(NULL);

    int grid_id;
    cin >> grid_id;

    vector<vector<int>> grid (300, vector<int> (300));
    int mx = -1e6;
    pair<int, int> best_coordinates;
    for (int x=0; x < 300; x++) {
        for (int y=0; y < 300; y++) {
            int rack_id = (x+1) + 10;
            int power = rack_id * (y+1);
            power += grid_id;
            power *= rack_id;
            power = (power/100) % 10;
            power -= 5;
            grid[x][y] = power;
            if (x >= 2 && y >= 2) {
                int tmp=0;
                for (int i=0; i <= 2; i++) {
                    for (int j=0; j <= 2; j++) {
                        tmp += grid[x-i][y-j];
                    }
                }
                if (amax(mx, tmp)) {
                    best_coordinates = {x+1, y+1};
                }
            }
        }
    }

    cout << best_coordinates.first-2 << "," << best_coordinates.second-2 << "\n";
}
