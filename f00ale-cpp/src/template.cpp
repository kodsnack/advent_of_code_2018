#include <iostream>
#include <vector>

int main() {
    int ans1 = 0;
    int ans2 = 0;

    {
        bool done = false;
        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                int x = c - '0';
            }

        }
    }


    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
