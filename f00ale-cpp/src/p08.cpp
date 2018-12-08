#include <iostream>
#include <vector>

struct Node {
    std::vector<Node> children;
    std::vector<int> data;
};
Node parse(std::vector<int>::const_iterator & it) {
    Node ret;
    int children = *(it++);
    int data = *(it++);
    for(int i = 0; i < children; i++) {
        ret.children.push_back(parse(it));
    }
    for(int i = 0; i < data; i++) {
        ret.data.push_back(*(it++));
    }
    return ret;
}

int calc1(const Node &n) {
    int ret = 0;
    for(auto c : n.children) {
        ret += calc1(c);
    }
    for(auto d : n.data) {
        ret += d;
    }
    return ret;
}

int calc2(const Node &n) {
    int ret = 0;
    if(n.children.empty()) {
        for(auto d : n.data) {
            ret += d;
        }
    } else {
        for(auto d : n.data) {
            if(d <= n.children.size()) {
                // we could memorize the sub-data, but things go fast enough without memorization
                ret += calc2(n.children[d - 1]);
            }
        }
    }

    return ret;
}



int main() {
    int ans1 = 0;
    int ans2 = 0;
    std::vector<int> input;
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
                if(have_num) {
                    input.push_back(num);
                }
                have_num = false;
                num = 0;
            }

        }
    }

    auto it = input.cbegin();
    Node root = parse(it);
    ans1 = calc1(root);
    ans2 = calc2(root);
    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
