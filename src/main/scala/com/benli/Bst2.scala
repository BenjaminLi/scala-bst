package com.benli

class Bst2[K, V](implicit ord: Ordering[K]) {
  private case class Node[K, V](
    left: Option[Node[K, V]],
    right: Option[Node[K, V]],
    k: K,
    v: V
  ) {
    override def toString: String = s"($k, $v)"
  }

  private var root: Option[Node[K, V]] = None

  def find(k: K): Option[V] = findNode(root, k).map(_.v)
  def insert[A](k: K, v: V): Unit = this.root = Some(insertNode(root, k, v));
  def delete[A](k: K): Unit = this.root = deleteNode(root, k);

  def size: Int = size(root)
  def min: Option[V] = minNode(root).map(_.v)
  def max: Option[V] = maxNode(root).map(_.v)
  def inorderString: String = inorderTraverse(root).mkString(", ")

  private def findNode(on: Option[Node[K, V]], key: K): Option[Node[K, V]] = on.flatMap{
    case Node(l, r, k, _) =>
      if (ord.equiv(k, key)) on
      else if (ord.lt(key, k)) findNode(l, key)
      else findNode(r, key)
  }

  private def insertNode(on: Option[Node[K, V]], key: K, value: V): Node[K, V] = on.map{
    case Node(l, r, k, _) if ord.equiv(k, key) => Node(l, r, k, value)
    case Node(l, r, k, v) if ord.lt(key, k) =>
      Node(Some(insertNode(l, key, value)), r, k, v)
    case Node(l, r, k, v) => Node(l, Some(insertNode(r, key, value)), k, v)
  }.getOrElse(Node(None, None, key, value))

  private def deleteNode(on: Option[Node[K, V]], key: K): Option[Node[K, V]] = on.flatMap{
    case Node(l, r, k, _) if ord.equiv(k, key) =>
      (l, r) match {
        case (None, None) => None
        case (None, r) => r
        case (l, None) => l
        case _ =>
          // elect the max node in the left branch to replace this node
          val node = maxNode(l).get
          val left = deleteNode(l, node.k)
          Some(Node(left, r, node.k, node.v))
      }
    case Node(l, r, k, v) if ord.lt(key, k) =>
      Some(Node(deleteNode(l, key), r, k, v))
    case Node(l, r, k, v) => Some(Node(l, deleteNode(r, key), k, v))
  }

  private def size(on: Option[Node[K, V]]): Int = on.map{ case Node(l, r, _, _) =>
    1 + size(l) + size(r)
  }.getOrElse(0)

  private def minNode(on: Option[Node[K, V]]): Option[Node[K, V]] = on.map{ n =>
    minNode(n.left).getOrElse(n)
  }

  private def maxNode(on: Option[Node[K, V]]): Option[Node[K, V]] = on.map{ n =>
    maxNode(n.right).getOrElse(n)
  }

  private def inorderTraverse(on: Option[Node[K, V]]): Seq[Node[K, V]] = on.map{ n =>
    (inorderTraverse(n.left) :+ n) ++ inorderTraverse(n.right)
   }.getOrElse(Seq())
}
