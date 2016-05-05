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
struct Tree
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
  explicit Tree(shared_ptr<const Node> const& node) : root_(node) {}

  Tree() {}; // empty tree
  Tree(Tree const& lft, T val, Tree const & rgt) :
    root_(make_shared<const Node>(lft.root_, val, rgt.root_))
  {
    assert(lft.isEmpty() || lft.root() < val);
    assert(rgt.isEmpty() || val < rgt.root());
  }
  Tree(initializer_list<T> init)
  {
    Tree t;
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
  Tree left() const 
  {
    assert(!isEmpty());
    return Tree(root_->lft);
  }
  Tree right() const 
  {
    assert(!isEmpty());
    return Tree(root_->rgt);
  }

  Tree insert(T x) const
  {
    if (isEmpty())
      return Tree(Tree(), x, Tree());
    T y = root();
    if (x<y)
        return Tree(left().insert(x), y, right());
    else if (y < x)
      return Tree(left(), y, right().insert(x));
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

  void walk() const
  {
    if (isEmpty())
      return;
    left().walk();
    cout << root_->val << endl;
    right().walk();

  }

};

int main()
{
  Tree<int> t{ 50, 40, 30, 10, 20, 30, 100, 0, 45, 55, 25, 15 };
  t.walk();
}
