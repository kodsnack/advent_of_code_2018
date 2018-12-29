//
// Created by Clayton Wong on 2018-12-17.
//

#pragma once


#include <vector>
#include <iostream>


using Register = std::vector< int >;


std::ostream& operator<<( std::ostream& os, const Register& R );
