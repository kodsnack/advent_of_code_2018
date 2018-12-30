//
// Created by Clayton Wong on 2018-12-17.
//

#include "Register.hpp"
#include <vector>
#include <iostream>


using namespace std;


ostream& operator<<( ostream& os, const Register& R )
{
    if( R.size() == 4 )
        os << "R[ 0 ] = " << R[ 0 ] << endl
           << "R[ 1 ] = " << R[ 1 ] << endl
           << "R[ 2 ] = " << R[ 2 ] << endl
           << "R[ 3 ] = " << R[ 3 ] << endl;
    return os;
}
