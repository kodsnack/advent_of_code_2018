//
// Created by Clayton Wong on 2018-12-20.
//

#include "input.hpp"
#include <iostream>
#include <string>
#include <vector>
#include <unordered_map>
#include <algorithm>


using namespace std;
using Room = tuple< int, int >;
struct Hash{ size_t operator()( const Room& room )const{ return abs( 0xABC * get<0>( room ) ) + abs( get<1>( room ) ); }};
using RoomCount = unordered_map< Room, int, Hash >;
using RoomStep = tuple< int, int, int >;
using Stack = vector< RoomStep >;


int main()
{
    Stack stack;
    RoomCount count;
    int step{ 0 }, row{ 0 }, col{ 0 };
    for( const auto c: INPUT )
        if( isalpha( c ) )
            row = ( 'N' == c )? row-1 : ( 'S' == c )? row+1 : row,
            col = ( 'E' == c )? col+1 : ( 'W' == c )? col-1 : col,
            count[{ row, col }] = max( count[{ row, col }], ++step );
        else if( '(' == c ) stack.push_back({ row, col, step });
        else if( '|' == c ) tie( row, col, step ) = stack.back();
        else if( ')' == c ) tie( row, col, step ) = stack.back(), stack.pop_back();

    const int
        max = max_element( count.cbegin(), count.cend(), []( const auto& p, const auto& q ){ return p.second < q.second; } )->second,
        above = count_if( count.cbegin(), count.cend(), []( const auto& p ){ return p.second > 999; });
    cout << "answer part 1: " << max << endl << "answer part 2: " << above << endl;

    return 0;
}
