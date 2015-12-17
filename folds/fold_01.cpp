#include <iostream>


int main(int argc, char * argv[])
{
  auto ForEach = [](auto&& fun, auto&&... args) 
  {
    (fun(args),...);
  };

  auto Printer = [](auto&& arg) { std::cout << arg; };

  ForEach(Printer, 1, " to the ", 2, " to the  ", 3, " \n");
  return 0;
}
