#include <iostream>
#include <vector>

int main() {
    std::string ans1;
    size_t ans2 = 0;
    int input = 0;
    int inputlen = 0;

    {
        bool done = false;
        int num = 0;
        bool have_num = false;
        int len = 0;
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
                len++;
            } else {
                if(have_num) {
                    input = num;
                    inputlen = len;
                }
                if(c == '\n') {

                }
                have_num = false;
                num = 0;
                len = 0;
            }

        }
    }

    const int mask = [](int num){
        int ret = 1;
        while(num) {
            ret *= 10;
            num /= 10;
        }
        return ret;
    }(input);

    auto updateseq = [mask](int seq, int n) {
        return (seq*10+n)%mask;
    };

    int e1 = 0, e2 = 1;

    std::vector<int> rcps = {3,7};
    rcps.reserve(30*1024*1024);
    int seq = 37;
    while(((int)rcps.size() < input + 10) || !ans2) {
        int n = rcps[e1]+rcps[e2];
        if(n>9) {
            rcps.push_back(n/10);
            seq = updateseq(seq, n/10);

            if(!ans2 && seq == input) ans2 = rcps.size() - inputlen;
        }
        rcps.push_back(n%10);
        seq = updateseq(seq, n%10);

        e1 += 1 + rcps[e1];
        while(e1 >= (int)rcps.size()) e1 -= rcps.size();
        e2 += 1 + rcps[e2];
        while(e2 >= (int)rcps.size()) e2 -= rcps.size();

        if(!ans2 && seq == input) ans2 = rcps.size()-inputlen;
    }

    for(int i = input; i < 10+input && i < (int)rcps.size(); i++) {
        ans1.push_back('0'+rcps[i]);
    }

    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
