#include <functional>
#include <utility>
#include <vector>
#include <algorithm>
#include <numeric>
#include<chrono>
#include <iostream>
#include <cassert>

template<class Vector, class F>
void sort(Vector &vector, F f)
{
  std::cout << __PRETTY_FUNCTION__ << std::endl;
  std::iota(vector.begin(), vector.end(), 0);
  std::random_shuffle(vector.begin(), vector.end());

  const auto start = std::chrono::system_clock::now();
  std::sort(vector.begin(), vector.end(), f);
  const auto end = std::chrono::system_clock::now();

  std::chrono::duration<double> elapsed_seconds = end-start;

  std::time_t end_time = std::chrono::system_clock::to_time_t(end);

  assert(std::is_sorted(vector.begin(), vector.end(),f));

  std::cout << "finished computation at " << std::ctime(&end_time)
    << "elapsed time: " << elapsed_seconds.count() << "s\n";
}

int main(void)
{
  auto lam1 = [=] (float x, float y) { return x>y;};
  
  auto wrapped = std::function<bool(float,float)>{lam1};

  std::vector<float> data(1000000);
  sort(data, lam1);
  sort(data, wrapped);

  return wrapped(1, 2);

}

