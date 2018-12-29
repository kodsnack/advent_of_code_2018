//
// Created by Clayton Wong on 2018-12-13.
//

#pragma once


#include "Map.hpp"
#include "Cart.hpp"
#include "Position.hpp"
#include <vector>
#include <set>


class Model
{
    Map map_;
    std::vector< Cart > carts_;

    void init() noexcept;

public:

    explicit Model( const std::string& input );
    ~Model() = default;
    Model( const Model& ) = delete;
    Model( Model&& ) = delete;
    Model& operator=( const Model& ) = delete;
    Model& operator=( Model&& ) = delete;

    bool isCollision() const noexcept;
    Position getCollisionPosition() const noexcept;
    int cartCount() const noexcept;

    void markCollisions() noexcept;
    void removeCollisions() noexcept;

    void tick() noexcept;
};
