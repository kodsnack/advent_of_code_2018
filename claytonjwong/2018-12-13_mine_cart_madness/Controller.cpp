#include "Input.hpp"
#include "Director.hpp"
#include "Cart.hpp"
#include "Model.hpp"
#include <iostream>


using namespace std;


int main() {

    Model model{ INPUT };
    while ( model.cartCount() > 1 )
    {
        model.tick();
    }

//    cout << model.getCollisionPosition() << endl; // (14,42), (8,7)
    return 0;
}
