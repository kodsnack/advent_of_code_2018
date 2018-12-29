//
// Created by Clayton Wong on 2018-12-13.
//

#include "Cart.hpp"
#include <iostream>


using namespace std;


size_t Cart::getID() const noexcept
{
    return id_;
}


Position Cart::getPosition() const noexcept
{
    return pos_;
}


Director::Direction Cart::getDirection() const noexcept
{
    return dir_;
}


char Cart::getLastTurn() const noexcept
{
    return turn_.last();
}


bool Cart::isCrashed() const noexcept
{
    return crashed_;
}


void Cart::setCrashed() noexcept
{
    crashed_ = true;
}


void Cart::advance( const Map& map ) noexcept
{
    updatePosition();
    updateDirection( map );
}


void Cart::updatePosition() noexcept
{
    //
    // relatively move forward by one position ( "forward" depends on direction )
    //
    if( dir_ == Director::Direction::up ) --pos_.y;
    else if( dir_ == Director::Direction::right ) ++pos_.x;
    else if( dir_ == Director::Direction::down ) ++pos_.y;
    else if( dir_ == Director::Direction::left ) --pos_.x;
}


void Cart::updateDirection( const Map& map ) noexcept
{
    char turnKey = map.get( pos_ );
    if( turnKey == '+' )
        turnKey = turn_.next();
    dir_ = Director::getNextDirection( dir_, turnKey );
}


bool operator<( const Cart& lhs, const Cart& rhs )
{
    return    ( lhs.getPosition().y < rhs.getPosition().y )
           || ( lhs.getPosition().y == rhs.getPosition().y && lhs.getPosition().x < rhs.getPosition().x );
}


std::ostream& operator<<( std::ostream& stream, const Cart& cart ) noexcept
{
    Position pos{ cart.getPosition() };
    Director::Direction dir{ cart.getDirection() };
    stream << "Cart[ " << cart.getID() << " ]: (" <<  pos.x << "," << pos.y << ") " << Director::to_char( dir ) << " " << cart.getLastTurn();
    return stream;
}
