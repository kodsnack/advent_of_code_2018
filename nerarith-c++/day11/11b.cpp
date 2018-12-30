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

    vector<vector<int>> grid (301, vector<int> (301)), sums (301, vector<int> (301, 0));
    int mx = -1e6;
    pair<int, int> best_coordinates;
    int best_size;
    for (int x=1; x <= 300; x++) {
        for (int y=1; y <= 300; y++) {
            int rack_id = x + 10;
            int power = rack_id * y;
            power += grid_id;
            power *= rack_id;
            power = (power/100) % 10;
            power -= 5;
            grid[x][y] = power;
            
            sums[x][y] = power + sums[x-1][y] + sums[x][y-1] - sums[x-1][y-1];
        }
    }

    for (int x=1; x <= 300; x++) {
        for (int y=1; y <= 300; y++) {
            int mx_size = min(x, y);
            for (int size=1; size <= mx_size; size++) {
                int tmp = sums[x][y] - sums[x-size][y] - sums[x][y-size] + sums[x-size][y-size];
                if (amax(mx, tmp)) {
                    best_coordinates = {x-size+1, y-size+1};
                    best_size = size;
                }
            }
        }
    }
    
    cout << best_coordinates.first << "," << best_coordinates.second << "," << best_size << "\n";
}
