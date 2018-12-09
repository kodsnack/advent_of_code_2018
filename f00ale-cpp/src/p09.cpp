#include <iostream>
#include <vector>
#include <algorithm>

int main() {
    int64_t ans1 = 0;
    int64_t ans2 = 0;
    int players = 0;
    int lastmarble = 0;
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
                    if(players) lastmarble = num;
                    else players = num;
                }
                have_num = false;
                num = 0;
            }

        }
    }

    struct Node {
        int value;
        Node *next;
        Node *prev;
    };

    Node *current = new Node;

    current->value = 0;
    current->next = current;
    current->prev = current;

    std::vector<int64_t> score(players);
    int firstmarble = 1;
    for(int outer = 0; outer < 100; outer++) {
        for (int i = firstmarble; i < lastmarble+firstmarble; i++) {
            if (i % 23 == 0) {
                int player = i % players;
                score[player] += i;
                for (int j = 0; j < 7; j++) {
                    current = current->prev;
                }
                score[player] += current->value;
                current->prev->next = current->next;
                current->next->prev = current->prev;
                auto tmp = current;
                current = current->next;
                delete tmp;
            } else {
                Node *n = new Node;
                n->value = i;
                current = current->next->next;

                current->prev->next = n;
                n->next = current;

                n->prev = current->prev;
                current->prev = n;

                current = n;
            }
        }
        if(!outer) {
            ans1 = *std::max_element(score.begin(), score.end());
        }
        firstmarble += lastmarble;
    }
    ans2 = *std::max_element(score.begin(), score.end());

    // cleanup
    current->prev->next = nullptr;
    while(current) {
        auto tmp = current;
        current = current->next;
        delete tmp;
    }


    std::cout << ans1 << std::endl;
    std::cout << ans2 << std::endl;
}
