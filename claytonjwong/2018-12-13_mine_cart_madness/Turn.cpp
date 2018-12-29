//
// Created by Clayton Wong on 2018-12-13.
//

#include "Turn.hpp"
#include <iostream>


using namespace std;


char Turn::next() noexcept
{
    last_ = ( last_ + 1 ) % 3; // continuously repeat 3 turning options: left, straight, right
    return last();
}


char Turn::last() const noexcept
{
    //
    // return char corresponding to the Director state machine
    // ( i.e. the '+' has 3 turning options: (L)eft, (S)traight, (R)ight
    //
    return ( last_ == left )? 'L' : ( last_ == straight )? 'S' : 'R';
}
