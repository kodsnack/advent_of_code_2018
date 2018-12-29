//
// Created by Clayton Wong on 2018-12-18.
//

#include "input.hpp"
#include <iostream>
#include <vector>
#include <string>
#include <sstream>
#include <algorithm>
#include <queue>
#include <unordered_set>


using namespace std;
const vector< vector< int > > ADJ{ {-1,0}, {0,-1}, {0,1}, {1,0} } ;
struct Cell{ char type{ '.' }; int row{ 0 }, col{ 0 }, attackPower{ 0 }, hitPoints{ 0 }; };
bool operator<( const Cell& lhs, const Cell& rhs ){ return lhs.row < rhs.row || ( lhs.row == rhs.row && lhs.col < rhs.col ); }
bool operator==( const Cell& lhs, const Cell& rhs ){ return lhs.row == rhs.row && lhs.col == rhs.col; }
struct Hash{ size_t operator()( const Cell& x ) const {  return x.row * 10000 + x.col; } };
using Units = vector< Cell >;
using Grid = vector< Units >;
using Unique = unordered_set< Cell, Hash >;


int main()
{
    pair< int, int > ans = { 0,0 };
    Grid G; int row{ 0 }, col{ 0 }; istringstream stream{ INPUT };
    for( string line; getline( stream, line ); ++row, col=0 )
    {
        G.push_back( {} );
        for( const auto c: line )
        {
            if( c == '.' || c == '#' )
                G.back().emplace_back( Cell{ c, row, col, 0, 0 } ); // empty cell (.) or wall (#)
            else
                G.back().emplace_back( Cell{ c, row, col, 3, 200 } ); // (E)lf or (G)oblin

            ++col;
        }
    }

    const auto M{ G.size() }, N{ G.back().size() };
    Units units;
    for( auto i{ 0 }; i < M; ++i )
        for( auto j{ 0 }; j < N; ++j )
            if( G[ i ][ j ].type == 'E' || G[ i ][ j ].type == 'G' )
                units.push_back( G[ i ][ j ] );
    for( auto round{ 0 };; ++round )
    {
        auto turns{ 0 }; auto islastDeathAfterLastTurn{ false };
        for( const auto& unit: units )
        {
            ++turns; // track the corner-case where an additional round should be counted when all turns
                     // have been performed, and the last death occurs on the last turn of the last round
                     // since this is checked against units.size(), count dead units turns as well

            //
            // process each alive unit as source
            //
            auto& source = G[ unit.row ][ unit.col ];
            if( source.type == '.' )
                continue; // source died already in this round

            //
            // attack ( if possible )
            //
            bool isAttack{ false };
            Units targets;
            for( const auto& a: ADJ )
            {
                row = source.row + a[ 0 ], col = source.col + a[ 1 ];
                auto& target = G[ row ][ col ];
                if( ( source.type == 'E' && target.type == 'G' )  ||  ( source.type == 'G' && target.type == 'E' ) )
                {
                    targets.push_back( target );
                    isAttack = true;
                }
            }
            if( isAttack )
            {
                auto weakestTarget{ targets.begin() };
                for( auto it{ targets.begin() }; it != targets.end(); ++it )
                {
                    if( weakestTarget->hitPoints > it->hitPoints )
                        weakestTarget = it;
                }
                auto& target = G[ weakestTarget->row ][ weakestTarget->col ];
                target.hitPoints -= source.attackPower;
                if( target.hitPoints <= 0 )
                {
                    if( turns == units.size() )
                        islastDeathAfterLastTurn = true;
                    target.type = '.',  target.attackPower = 0, target.hitPoints = 0; // target died
                }

                continue; // no movement performed if attack
            }

            //
            // move ( BFS ) to find closest target(s)
            //
            targets.clear(); Unique V({ source }); // (V)isited cells
            for( queue< Cell > q({ source }); targets.empty() && ! q.empty(); )
            {
                for( auto depth{ q.size() }; depth--; ) // perform BFS one depth at a time
                {
                    const auto& cur = q.front(); q.pop();
                    for( const auto& a: ADJ )
                    {
                        row = cur.row + a[ 0 ], col = cur.col + a[ 1 ];
                        const auto& candidate = G[ row ][ col ];
                        if( candidate.type == '.' && V.insert( candidate ).second )
                            q.push( candidate );
                        else if( ( source.type == 'E' && candidate.type == 'G' )  ||  ( source.type == 'G' && candidate.type == 'E' ) )
                            targets.push_back( candidate );
                    } // for each current cell adjacent cells
                } // for each depth in the BFS
            } // loop until target is found ( move towards it next ) or no other candidates to perform BFS upon ( no-op )
            if( targets.empty() )
                continue; // no movement performed if target not found

            //
            // find all adjacent cells from targets to find shortest path back to the source
            //
            Unique adjTarget;
            for( const auto& target: targets )
            {
                for( const auto& a: ADJ )
                {
                    row = target.row + a[ 0 ], col = target.col + a[ 1 ];
                    const auto& candidate = G[ row ][ col ];
                    if( candidate.type == '.' )
                        adjTarget.insert( candidate );
                }
            }

            //
            // perform BFS in reverse from destination back to source
            // in order to choose a source move to make based on the shortest distance to the destination
            // from each cell adjacent to the source
            //
            Unique adjSource;
            for( const auto a: ADJ )
            {
                row = source.row + a[ 0 ], col = source.col + a[ 1 ];
                const auto& candidate = G[ row ][ col ];
                if( candidate.type == '.' )
                    adjSource.insert( candidate );
            }
            Units moves; V.clear(); V.insert( adjTarget.cbegin(), adjTarget.cend() ); // (V)isited cells
            for( queue< Cell > q({ adjTarget.cbegin(), adjTarget.cend() }); moves.empty() && ! q.empty(); )
            {
                for( auto depth{ q.size() }; depth--; ) // perform BFS one depth at a time
                {
                    Cell cur = q.front(); q.pop();
                    if( adjSource.find( cur ) != adjSource.end() )
                    {
                        moves.push_back( cur );
                        continue;
                    }
                    for( const auto a: ADJ )
                    {
                        row = cur.row + a[ 0 ], col = cur.col + a[ 1 ];
                        const auto& candidate = G[ row ][ col ];
                        if( candidate.type == '.' && V.insert( candidate ).second )
                            q.push( candidate );
                    } // for each current cell adjacent cells
                } // for each depth in the BFS
            } // loop until move is found ( move towards it next ) or no other candidates to perform BFS upon ( no-op )

            //
            // choose source move and perform move by swapping all values except the row/col ( which remains the same for each cell )
            //
            sort( moves.begin(), moves.end() ); // reading order, so choose the first sorted move
            auto& move = G[ moves[ 0 ].row ][ moves[ 0 ].col ];
            swap( source.type, move.type );
            swap( source.attackPower, move.attackPower );
            swap( source.hitPoints, move.hitPoints );

            //
            // attack from move ( if possible ) -- [ Note: source is a reference which still refers to the "old" source position
            //                                             currently move is actually source, since they were swapped during the move ]
            //
            for( const auto a: ADJ )
            {
                row = move.row + a[ 0 ], col = move.col + a[ 1 ];
                auto& target = G[ row ][ col ];
                if( ( move.type == 'E' && target.type == 'G' )  ||  ( move.type == 'G' && target.type == 'E' ) )
                {
                    target.hitPoints -= move.attackPower;
                    if( target.hitPoints <= 0 )
                    {
                        target.type = '.',  target.attackPower = 0, target.hitPoints = 0; // target died
                        if( turns == units.size() )
                            islastDeathAfterLastTurn = true;
                    }

                    break;
                }
            }
        } // for each unit's turn this round

        auto lastUnitSize = units.size();
        units.clear();
        for( auto i{ 0 }; i < M; ++i )
            for( auto j{ 0 }; j < N; ++j )
                if( G[ i ][ j ].type == 'E' || G[ i ][ j ].type == 'G' )
                    units.push_back( G[ i ][ j ] );
        if( count_if( units.cbegin(), units.cend(), []( const auto& cell ){ return cell.type == 'E'; }) == 0 ||
            count_if( units.cbegin(), units.cend(), []( const auto& cell ){ return cell.type == 'G'; }) == 0    )
        {
            if( islastDeathAfterLastTurn ) // count the last round if the last death to end the game
                ++round;                   // occurred on the last turn of the last round

            cout << "Game END: " << round << endl;
            for( auto i{ 0 }; i < M; cout << endl, ++i )
                for( auto j{ 0 }; j < N; ++j )
                    cout << G[ i ][ j ].type;
            cout << endl;

            int hitPointTotal{ 0 };
            for( const auto& unit: units )
            {
                hitPointTotal += unit.hitPoints;
                cout << "hitPoints: " << unit.hitPoints << endl;
            }
            cout << "hitPoint total: " << hitPointTotal << endl;
            cout << "rounds: " << round << endl;
            ans.first = hitPointTotal * round;

            break; // GAME OVER: no more (E)lfs or (G)oblins left, end battle without counting this round
        }

    } // for each round

    cout << "answer part 1: " << ans.first << endl;

    return 0;
}
