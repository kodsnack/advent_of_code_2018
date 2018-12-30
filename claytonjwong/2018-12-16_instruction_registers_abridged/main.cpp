//
// Created by Clayton Wong on 2018-12-17.
//

#include "input.hpp"
#include <iostream>
#include <vector>
#include <memory>
#include <regex>
#include <sstream>
#include <unordered_set>


using namespace std;
using Register = vector< int >;
using Command = tuple< int, int, int, int >;
using Test = tuple< Register, Command, Register >; // before register, command, after register
using Tests = vector< Test >;


class Solution
{
    regex
        registerBeforePattern{ "^Before:\\s*\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]$" },
        commandPattern{ "^\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)\\s*$" },
        registerAfterPattern{ "^After:\\s*\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]$" },
        whitespacePattern{ "^\\s*$" };

    int opcode{ 0 }, A{ 0 }, B{ 0 }, C{ 0 };

    Register addr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] + R[ B ];
        return R;
    }

    Register addi( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] + B;
        return R;
    }

    Register mulr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] * R[ B ];
        return R;
    }

    Register muli( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] * B;
        return R;
    }

    Register banr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] & R[ B ];
        return R;
    }

    Register bani( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] & B;
        return R;
    }

    Register borr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] | R[ B ];
        return R;
    }

    Register bori( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ] | B;
        return R;
    }

    Register setr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = R[ A ];
        return R;
    }

    Register seti( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = A;
        return R;
    }

    Register gtir( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = ( A > R[ B ] )? 1 : 0;
        return R;
    }

    Register gtri( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = ( R[ A ] > B )? 1 : 0;
        return R;
    }

    Register gtrr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = ( R[ A ] > R[ B ] )? 1 : 0;
        return R;
    }

    Register eqir( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = ( A == R[ B ] )? 1 : 0;
        return R;
    }

    Register eqri( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = ( R[ A ] == B )? 1 : 0;
        return R;
    }

    Register eqrr( const Register& in, const Command& cmd )
    {
        Register R{ in }; tie( opcode, A, B, C ) = cmd;

        R[ C ] = ( R[ A ] == R[ B ] )? 1 : 0;
        return R;
    }

public:

    Tests getTests( const string& sampleInput, int _1=0, int _2=0, int _3=0, int _4=0, Tests tests={} ) const noexcept
    {
        istringstream is{ sampleInput };
        for( string beforeStr, commandStr, afterStr, whitespaceStr;
             getline( is, beforeStr ) && getline( is, commandStr ) && getline( is, afterStr ) && getline( is, whitespaceStr );  )
        {
            smatch before, command, after, whitespace;
            if(    regex_match( beforeStr, before, registerBeforePattern ) && before.size() == 5
                   && regex_match( commandStr, command, commandPattern ) && command.size() == 5
                   && regex_match( afterStr, after, registerAfterPattern ) && after.size() == 5
                   && regex_match( whitespaceStr, whitespace, whitespacePattern ) && whitespace.size() == 1 )
            {
                stringstream parser;
                parser << before[ 1 ] << ' ' << before[ 2 ] << ' ' << before[ 3 ] << ' ' << before[ 4 ];
                parser >> _1 >> _2 >> _3 >> _4; Register in{ _1, _2, _3, _4 };
                parser.clear();

                parser << command[ 1 ] << ' ' << command[ 2 ] << ' ' << command[ 3 ] << ' ' << command[ 4 ];
                parser >> _1 >> _2 >> _3 >> _4; Command cmd{ _1, _2, _3, _4 }; // a,b,c
                parser.clear();

                parser << after[ 1 ] << ' ' << after[ 2 ] << ' ' << after[ 3 ] << ' ' << after[ 4 ];
                parser >> _1 >> _2 >> _3 >> _4; Register out{ _1, _2, _3, _4 };
                parser.clear();

                tests.push_back({ in, cmd, out });
            }
        }
        return tests;
    }

    int getCommandMatchCount( Test& test )
    {
        int match{ 0 };
        Register in, out;
        Command cmd;
        tie( in, cmd, out ) = test;

        if( addr( in, cmd ) == out ) ++match; //, cout << "addr  9" << endl;
        if( addi( in, cmd ) == out ) ++match; //, cout << "addi 10" << endl;
        if( mulr( in, cmd ) == out ) ++match; //, cout << "mulr  1" << endl;
        if( muli( in, cmd ) == out ) ++match; //, cout << "muli 15" << endl;
        if( banr( in, cmd ) == out ) ++match; //, cout << "banr  6" << endl;
        if( bani( in, cmd ) == out ) ++match; //, cout << "bani  8" << endl;
        if( borr( in, cmd ) == out ) ++match; //, cout << "borr  5" << endl;
        if( bori( in, cmd ) == out ) ++match; //, cout << "bori  4" << endl;
        if( setr( in, cmd ) == out ) ++match; //, cout << "setr 14" << endl;
        if( seti( in, cmd ) == out ) ++match; //, cout << "seti  2" << endl;
        if( gtir( in, cmd ) == out ) ++match; //, cout << "gtir  0" << endl;
        if( gtri( in, cmd ) == out ) ++match; //, cout << "gtri 12" << endl;
        if( gtrr( in, cmd ) == out ) ++match; //, cout << "gtrr  3" << endl;
        if( eqir( in, cmd ) == out ) ++match; //, cout << "eqir 13" << endl;
        if( eqri( in, cmd ) == out ) ++match; //, cout << "eqri  7" << endl;
        if( eqrr( in, cmd ) == out ) ++match; //, cout << "eqrr 11" << endl;

        return match;
    }


    int getMatchingOpcodeCount( const string& input, int threshold, int ans=0 )
    {
        auto tests = getTests( input );
        for( auto& test: tests )
        {
            int matches = getCommandMatchCount( test );
            if( matches >= threshold )
                ++ans;
        }
        return ans;
    }

    int getIncrementalMatches( const string& input, int threshold, int ans=0 )
    {
        auto tests = getTests( input );
        for( auto& test: tests )
            for( int target{ 0 }; target <= threshold; ++target )
                if( target == getCommandMatchCount( test ) )
                {
                    auto cmd = get<1>( test );
                    cout << ": " << target << " matches for opcode " << get<0>( cmd ) << endl;
                }
        return ans;

//        eqrr 11
//        : 1 matches for opcode 11

//        eqri
//        eqrr 11
//        : 2 matches for opcode 7

//        eqir
//        eqri  7
//        eqrr 11
//        : 3 matches for opcode 13

//        gtri
//        eqir 13
//        eqrr 11
//        : 3 matches for opcode 12

//        gtri 12
//        gtrr
//        eqir 13
//        eqri  7
//        : 4 matches for opcode 3

//        gtir
//        gtri 12
//        gtrr  3
//        eqir 13
//        eqri  7
//        eqrr 11
//        : 6 matches for opcode 0

//        banr
//        gtir  0
//        gtri 12
//        gtrr  3
//        eqir 13
//        eqri  7
//        eqrr 11
//        : 7 matches for opcode 6

//        setr
//        gtir  0
//        : 2 matches for opcode 14

//        banr  6
//        bani
//        gtir  0
//        gtri 12
//        gtrr  3
//        eqir 13
//        eqri  7
//        eqrr 11
//        : 8 matches for opcode 8

//        banr  6
//        bani  8
//        seti
//        gtir  0
//        gtri 12
//        gtrr  3
//        eqir 13
//        eqri  7
//        eqrr 11
//        : 9 matches for opcode 2

//        addr
//        seti  2
//        : 2 matches for opcode 9

//        addr  9
//        bani  8
//        borr
//        setr 14
//        seti  2
//        gtir  0
//        gtrr  3
//        : 7 matches for opcode 5

//        banr  6
//        bani  8
//        borr  5
//        bori
//        setr 14
//        seti  2
//        : 6 matches for opcode 4

//        addr  9
//        muli
//        bani  8
//        borr  5
//        bori  4
//        setr 14
//        gtir  0
//        gtrr  3
//        eqri  7
//        : 9 matches for opcode 15

//        addr  9
//        addi
//        borr  5
//        bori  4
//        setr 14
//        seti  2
//        gtir  0
//        gtri 12
//        gtrr  3
//        : 9 matches for opcode 10

//        addr  9
//        addi 10
//        mulr
//        muli 15
//        banr  6
//        bani  8
//        borr  5
//        bori  4
//        setr 14
//        seti  2
//        gtir  0
//        gtri 12
//        gtrr  3
//        : 13 matches for opcode 1
    }

    Register testProgram( const string& input, Register R={ 0,0,0,0 } )
    {
        istringstream stream{ input };
        int opcode{ 0 }, A{ 0 }, B{ 0 }, C{ 0 };
        for( string line; getline( stream, line ); )
        {
            smatch group;
            if( regex_match( line, group, commandPattern ) && group.size() == 5 )
            {
                stringstream parser;
                parser << group[ 1 ] << ' ' << group[ 2 ] << ' ' << group[ 3 ] << ' ' << group[ 4 ];
                parser >> opcode >> A >> B >> C;
                Command cmd{ opcode, A, B, C };

                if( opcode ==  0 ) R = gtir( R, cmd );
                if( opcode ==  1 ) R = mulr( R, cmd );
                if( opcode ==  2 ) R = seti( R, cmd );
                if( opcode ==  3 ) R = gtrr( R, cmd );
                if( opcode ==  4 ) R = bori( R, cmd );
                if( opcode ==  5 ) R = borr( R, cmd );
                if( opcode ==  6 ) R = banr( R, cmd );
                if( opcode ==  7 ) R = eqri( R, cmd );
                if( opcode ==  8 ) R = bani( R, cmd );
                if( opcode ==  9 ) R = addr( R, cmd );
                if( opcode == 10 ) R = addi( R, cmd );
                if( opcode == 11 ) R = eqrr( R, cmd );
                if( opcode == 12 ) R = gtri( R, cmd );
                if( opcode == 13 ) R = eqir( R, cmd );
                if( opcode == 14 ) R = setr( R, cmd );
                if( opcode == 15 ) R = muli( R, cmd );
            }
        }
        return R;
    }

};

int main()
{
    Solution s;
    cout << "answer part 1: " << s.getMatchingOpcodeCount( SAMPLE_INPUT, 3 ) << endl;

    auto output{ s.testProgram( TEST_PROGRAM_INPUT ) };
    cout << "answer part 2: " << output[ 0 ] << endl;

//    answer part 1: 618
//    answer part 2: 514
//
//    Process finished with exit code 0

    return 0;
}
