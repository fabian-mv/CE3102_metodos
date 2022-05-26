#include <iostream>
#include <armadillo>
#include <math.h>

#include "thomas.h"
#include "gradiente_conjugado.h"

using namespace std;
using namespace arma;

int main(){

  mat A = {  { 5,  1, 0,  0},
             { 1,  5, 1,  0},
             { 0,  1, 5,  1},
             { 0,  0, 1,  5}};

  vec b = {6, 7, 7, 6};

  cout << "---------------------Thomas------------------- \n" <<endl;

  vec t = thomas(A,b);

  cout << t <<endl;

  cout << "---------------------Gradiente conjugado------------------- \n" <<endl;

  vec g = gradiente_conjugado(A,b);

#ifdef WIN32
  system("pause");
#endif
  
  return 0;
}
