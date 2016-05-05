#include <memory>
#include <initializer_list>
#include <cassert>
#include <iostream>
using std::shared_ptr;
using std::make_shared;
using std::initializer_list;
using std::cout;
using std::endl;

template<class T>
struct RBTree
{
  struct Node
  {
    shared_ptr<const Node> lft;
    T val;
    shared_ptr<const Node> rgt;
    Node(shared_ptr<const Node> const & lft, T val, shared_ptr<const Node> const &rgt) :
        lft(lft), val(val), rgt(rgt) {}

  };
  shared_ptr<const Node> root_;
  explicit RBTree(shared_ptr<const Node> const& node) : root_(node) {}

  RBTree() {}; // empty tree
  RBTree(RBTree const& lft, T val, RBTree const & rgt) :
    root_(make_shared<const Node>(lft.root_, val, rgt.root_))
  {
    assert(lft.isEmpty() || lft.root() < val);
    assert(rgt.isEmpty() || val < rgt.root());
  }
  RBTree(initializer_list<T> init)
  {
    RBTree t;
    for (T v : init)
    {
      t = t.insert(v);
    }
    root_ = t.root_;
  }

  bool isEmpty() const { return !root_; }
  T root() const 
  {
    assert(!isEmpty());
    return root_->val;
  }
  RBTree left() const 
  {
    assert(!isEmpty());
    return RBTree(root_->lft);
  }
  RBTree right() const 
  {
    assert(!isEmpty());
    return RBTree(root_->rgt);
  }

  RBTree insert(T x) const
  {
    if (isEmpty())
      return RBTree(RBTree(), x, RBTree());
    T y = root();
    if (x<y)
        return RBTree(left().insert(x), y, right());
    else if (y < x)
      return RBTree(left(), y, right().insert(x));
    else
      return *this; /* no duplicates */
  }

  bool member(T x) const
  {
    if (isEmpty())
      return false;
    T y = root();
    if (x < y)
      return left().member(x);
    else if (y < x)
      return right().member(x);
    else
      return true;
  }

  void walk(int depth = 0) const
  {
    if (isEmpty())
      return;
    left().walk(depth+1);
    cout << "depth= " << depth << "  value= " << root_->val << endl;
    right().walk(depth+1);

  }

};

int main()
{
  RBTree<int> t{ 50, 40, 30, 10, 20, 30, 100, 0, 45, 55, 25, 15 };
  t.walk();
  cout << endl;
  RBTree<int> t1{ 50, 40, 30, 20, 10, 9,8,7,6,5,4,3,2,1};
  t1.walk();
}
