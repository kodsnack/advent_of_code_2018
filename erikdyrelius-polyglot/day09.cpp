#include <deque>
#include <map>
#include <iostream>
#include <cstdint>

using namespace std;

class Marbles {
private:
    deque<uint64_t> board;
public:
    Marbles() {
        board.push_front(0);
    }
    void rotate(int steps) {
        while (steps > 0) {
            board.push_back(board.front());
            board.pop_front();
            steps--;
        }
        while (steps < 0) {
            board.push_front(board.back());
            board.pop_back();
            steps++;
        }
    }
    uint64_t play(uint64_t nofMarbles, uint32_t elves) {
        uint32_t theMarble = 1U, theElf = 1U;
        map<uint32_t, uint64_t> score;
        for (uint32_t elf = 1U; elf <= elves; ++elf) score[elf] = 0U;
        while (theMarble < nofMarbles) {
            if (theMarble % 23U == 0U) {
                rotate(7);
                score[theElf] += theMarble + board.front();
                board.pop_front();
            } else {
                rotate(-2);
                board.push_front(theMarble);
            }
            theElf = (theElf % nofMarbles) + 1U;
            theMarble++;
        }
        uint64_t mx = 0U;
        for (uint32_t elf = 1U; elf <= elves; ++elf) mx = score[elf]>mx ? score[elf] : mx;
        return mx;
    }
};

int main() {
    Marbles m1;
    uint64_t sol1 = m1.play(71863, 493);
    cout << sol1 << endl;
    return 0;
}