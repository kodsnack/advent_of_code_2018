//
// Created by Clayton Wong on 2018-12-17.
//

#include "input.hpp"
#include "Register.hpp"
#include "Command.hpp"
#include "CommandSuite.hpp"
#include <iostream>
#include <vector>
#include <memory>
#include <regex>
#include <sstream>
#include <unordered_set>


using namespace std;


class Solution
{
    regex
        registerBeforePattern{ "^Before:\\s*\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]$" },
        commandPattern{ "^\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)\\s*(\\d+)\\s*$" },
        registerAfterPattern{ "^After:\\s*\\[\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*,\\s*(\\d+)\\s*\\]$" },
        whitespacePattern{ "^\\s*$" };
public:

    int getMatchingOpcodeCount( const string& sampleInput, const int threshold, int _1=0, int _2=0, int _3=0, int _4=0, int ans=0 ) const noexcept
    {
        istringstream is{ sampleInput };
        for( string before, command, after, whitespace;
             getline( is, before ) && getline( is, command ) && getline( is, after ) && getline( is, whitespace );  )
        {
            smatch registerBeforeGroup, commandGroup, registerAfterGroup, whitespaceGroup;
            if(    regex_match( before, registerBeforeGroup, registerBeforePattern ) && registerBeforeGroup.size() == 5
                && regex_match( command, commandGroup, commandPattern ) && commandGroup.size() == 5
                && regex_match( after, registerAfterGroup, registerAfterPattern ) && registerAfterGroup.size() == 5
                && regex_match( whitespace, whitespaceGroup, whitespacePattern ) && whitespaceGroup.size() == 1 )
            {
                stringstream parser;
                parser << registerBeforeGroup[ 1 ] << ' ' << registerBeforeGroup[ 2 ] << ' ' << registerBeforeGroup[ 3 ] << ' ' << registerBeforeGroup[ 4 ];
                parser >> _1 >> _2 >> _3 >> _4;
                Register input{ _1, _2, _3, _4 };
                parser.clear();

                parser << commandGroup[ 1 ] << ' ' << commandGroup[ 2 ] << ' ' << commandGroup[ 3 ] << ' ' << commandGroup[ 4 ];
                parser >> _1 >> _2 >> _3 >> _4;
                Command testData{ _1, _2, _3, _4 }; // a,b,c
                parser.clear();

                parser << registerAfterGroup[ 1 ] << ' ' << registerAfterGroup[ 2 ] << ' ' << registerAfterGroup[ 3 ] << ' ' << registerAfterGroup[ 4 ];
                parser >> _1 >> _2 >> _3 >> _4;
                Register target{ _1, _2, _3, _4 };
                parser.clear();

                CommandSuite testSuite{ testData.opcode, testData.A, testData.B, testData.C };
                CommandSet unique;
                for( const auto& test: testSuite )
                {
                    Register output{ test->execute( input ) };
                    if( output == target )
                        unique.push_back( test );
                }
                ans += unique.size() >= threshold ? 1 : 0; // part 1: add one onto the answer if the # of matches >= threshold

                // part 2: output used for deductive reasoning by comparing the non-ambiguous remaining unassigned opcodes, one-by-one
                // TODO: part 2 answer is not accepted ( 0 is too low ), double check to ensure proper opcode mapping here
//                for( int i{ 0 }; i < 16; ++i )
//                {
//                    if( unique.size() == i )
//                    {
//                        for( const auto test: unique )
//                            cout << i << " choices for opcode: " << test->opcode << " == " << test->getName() << endl;
//                        cout << endl << endl;
//                    }
//                }
            }
        }
        return ans;
    }

    Register testProgram( const string& input, Register R={ 0,0,0,0 }, int op=0, int a=0, int b=0, int c=0 ) const noexcept
    {
        CommandSuite suite;

        istringstream stream{ input };
        for( string line; getline( stream, line ); )
        {
            smatch group;
            if( regex_match( line, group, commandPattern ) && group.size() == 5 )
            {
                stringstream parser;
                parser << group[ 1 ] << ' ' << group[ 2 ] << ' ' << group[ 3 ] << ' ' << group[ 4 ];
                parser >> op >> a >> b >> c;
                R = suite.Execute( R, op, a, b, c );
            };
        }
        return R;
    }
};

int main()
{
    Solution s;
    cout << s.getMatchingOpcodeCount( SAMPLE_INPUT, 3 ) << endl;
    auto output{ s.testProgram( TEST_PROGRAM_INPUT ) }; // TODO: part 2 -- wrong answer
    cout << output << endl;

    return 0;
}