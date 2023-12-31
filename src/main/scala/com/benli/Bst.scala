package com.benli

class Bst[K, V](implicit ord: Ordering[K]) {
  private sealed trait Tree[K, V] {
    def valueOption: Option[V]
    def toString: String
  }
  private case object Leaf extends Tree[K, V] {
    def valueOption: Option[V] = None
    override def toString: String = ""
  }
  private case class Branch[K, V](
     left: Tree[K, V],
     right: Tree[K, V],
     k: K,
     v: V
   ) extends Tree[K, V] {
    def valueOption: Option[V] = Some(v)
    override def toString: String = s"($k, $v)"
  }

  private var root: Tree[K, V] = Leaf

  def find(k: K): Option[V] = findNode(root, k).valueOption
  def insert[A](k: K, v: V): Unit = this.root = insertNode(root, k, v);
  def delete[A](k: K): Unit = this.root = deleteNode(root, k);

  def size: Int = size(root)
  def min: Option[V] = minNode(root).valueOption
  def max: Option[V] = maxNode(root).valueOption
  def inorderString: String = inorderTraverse(root)

  @annotation.tailrec
  private def findNode(t: Tree[K, V], key: K): Tree[K, V] = t match {
    case Leaf => Leaf
    case Branch(l, r, k, _) =>
      if (ord.equiv(k, key)) t
      else if (ord.lt(key, k)) findNode(l, key)
      else findNode(r, key)
  }

  private def insertNode(t: Tree[K, V], key: K, value: V): Tree[K, V] = t match {
    case Leaf => Branch(Leaf, Leaf, key, value)
    case Branch(l, r, k, _) if ord.equiv(k, key) => Branch(l, r, k, value)
    case Branch(l, r, k, v) if ord.lt(key, k) =>
      Branch(insertNode(l, key, value), r, k, v)
    case Branch(l, r, k, v) => Branch(l, insertNode(r, key, value), k, v)
  }

  private def deleteNode(t: Tree[K, V], key: K): Tree[K, V] = t match {
    case Leaf => Leaf
    case Branch(l, r, k, _) if ord.equiv(k, key) =>
      (l, r) match {
        case (Leaf, Leaf) => Leaf
        case (Leaf, r) => r
        case (l, Leaf) => l
        case _ =>
          // elect the max node in the left branch to replace this node
          val node = maxNode(l).asInstanceOf[Branch[K, V]]
          val left = deleteNode(l, node.k)
          Branch(left, r, node.k, node.v)
      }
    case Branch(l, r, k, v) if (ord.lt(key, k)) =>
      Branch(deleteNode(l, key), r, k, v)
    case Branch(l, r, k, v) => Branch(l, deleteNode(r, key), k, v)
  }

  private def size(t: Tree[K, V]): Int = t match {
    case Leaf => 0
    case Branch(l, r, _, _) => 1 + size(l) + size(r)
  }

  private def minNode(t: Tree[K, V]): Tree[K, V] = t match {
    case Branch(l, _, _, _) => l match {
      case Leaf => t
      case _ => minNode(l)
    }
    case Leaf => Leaf
  }

  private def maxNode(t: Tree[K, V]): Tree[K, V] = t match {
    case Branch(_, r, _, _) => r match {
      case Leaf => t
      case _ => maxNode(r)
    }
    case Leaf => Leaf
  }

  private def inorderTraverse(t: Tree[K, V]): String = t match {
    case Branch(Leaf, Leaf, _, _) => t.toString
    case Branch(Leaf, r, _, _) => s"$t, ${inorderTraverse(r)}"
    case Branch(l, Leaf, _, _) => s"${inorderTraverse(l)}, $t"
    case Branch(l, r, _, _) => s"${inorderTraverse(l)}, $t, ${inorderTraverse(r)}"
    case Leaf => Leaf.toString
  }
}
