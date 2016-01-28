#include <iostream>
#include <cassert>
using namespace std;

struct TreeNode
{
  enum Kind {RED, BLUE};

  TreeNode(Kind kind, TreeNode *left = nullptr, TreeNode* right=nullptr) :
    kind(kind), left(left), right(right) {}

  Kind kind;
  TreeNode *left, *right;
};

template<class Derived>
class GenericVisitor
{
  public:
    void visit_preorder(TreeNode* node)
    {
      if (node) 
      {
        dispatch_node(node);
        visit_preorder(node->left);
        visit_preorder(node->right);
      }
    }
    void visit_inorder(TreeNode* node)
    {
      if (node) 
      {
        visit_preorder(node->left);
        dispatch_node(node);
        visit_preorder(node->right);
      }
    }
    void visit_postorder(TreeNode* node)
    {
      if (node) 
      {
        visit_preorder(node->left);
        visit_preorder(node->right);
        dispatch_node(node);
      }
    }

    void handle_RED(TreeNode* node)
    {
      cerr << "Generic handle RED\n";
    }
    void handle_BLUE(TreeNode* node)
    {
      cerr << "Generic handle BLUE\n";
    }

  private:
    Derived& derived()
    {
      return *static_cast<Derived*>(this);
    }

    void dispatch_node(TreeNode* node)
    {
      switch (node->kind)
      {
        case TreeNode::RED:
          derived().handle_RED(node);
          break;
        case TreeNode::BLUE:
          derived().handle_BLUE(node);
          break;
        default:
          assert(0);
      };
    }
};

class SpecialVisitor : public GenericVisitor<SpecialVisitor>
{
  public:
    void handle_RED(TreeNode* node)
    {
      cerr << "RED is special\n";
    }
};

class BasicVisitor : public GenericVisitor<BasicVisitor>
{
};

int main()
{
  auto root = new TreeNode(TreeNode::RED);
  auto l = new TreeNode(TreeNode::RED);
  auto r = new TreeNode(TreeNode::BLUE);
  root->left = l;
  root->right = r;
  l = new TreeNode(TreeNode::BLUE);
  r = new TreeNode(TreeNode::BLUE);
  root->left = l;
  root->right = r;
  l = new TreeNode(TreeNode::RED);
  r = new TreeNode(TreeNode::RED);
  root->left->left = r;
  root->right->right = l;

  {
    SpecialVisitor v;
    v.visit_preorder(root);
  }
  {
    BasicVisitor v;
    v.visit_preorder(root);
  }
}
