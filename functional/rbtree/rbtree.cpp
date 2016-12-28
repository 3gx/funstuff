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
  enum Color {R, B};
  struct Node
  {
    Color c;
    shared_ptr<const Node> lft;
    T val;
    shared_ptr<const Node> rgt;
    Node(Color c, shared_ptr<const Node> const &lft, T val,
         shared_ptr<const Node> const &rgt)
        : c(c), lft(lft), val(val), rgt(rgt)
    {
    }
  };
  shared_ptr<const Node> root_;
  explicit RBTree(shared_ptr<const Node> const& node) : root_(node) {}

  RBTree() {}; // empty tree
  RBTree(Color c, RBTree const& lft, T val, RBTree const & rgt) :
    root_(c, make_shared<const Node>(lft.root_, val, rgt.root_))
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
  Color rootColor() const
  {
    assert(!isEmpty());
    return root_->c;
  }

#if 0
  RBTree insert_unballanced(T x) const
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
#endif

  RBTree insert(T x) const
  {
    RBTree t = ins(x);
    return RBTree(B, t.left(), t.root(), t.right());
  }

  RBTree ins(T x) const
  {
    if (is_Rmpty())
      return RBTree(R, RBTree(), x, RBTree());
    T y = root();
    Color c = rootColor();
    if (x < y)
      return balance(c, left().ins(x), y, right());
    else if (y < x)
      return balance(c, left(), y, right().ins(x));
    else
      return *this; // no duplicates
  }

  static RBTree balance(Color c, RBTree const &lft, T x, RBTree const &rgt) 
  {
    if (c == B && lft.doubleLeft())
    {
      return RBTree(R, lft.left().paint(B), lft.root(),
                    RBTree(B, lft.rigth(), x, rgt));
    }
    else if (c == B && lft.doubledRight())
    {
      return RBTree(R, RBTree(
    }
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
