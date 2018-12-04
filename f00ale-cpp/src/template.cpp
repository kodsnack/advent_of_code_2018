#include <iostream>
#include <vector>

int main() {
    int ans1 = 0;
    int ans2 = 0;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;

        while (!done) {
            char c;
            std::cin.get(c);
            if (!std::cin.good()) {
                done = true;
                c = '\n';
            }

            if(c >= '0' && c <= '9') {
                num *= 10;
                num += c - '0';
                have_num = true;
            } else {
                if(c == '\n') {

                }
                have_num = false;
                num = 0;
            }

        }
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
