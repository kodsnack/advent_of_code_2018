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

int main() {
    string id;
    int boxes_two=0, boxes_three=0;
    while (cin >> id) {
        bool found_two=false, found_three=false;
        for (char c='a'; c <= 'z' && (!found_two || !found_three); c++) {
            int cnt = count(all(id), c);
            
            if (cnt == 2 && !found_two) {
                boxes_two++;
                found_two = true;
            }
            else if (cnt == 3 && !found_three) {
                boxes_three++;
                found_three = true;
            }
        }
    }
    cout << boxes_two * boxes_three << "\n";
}
