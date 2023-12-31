package com.benli

import org.scalatest._

class BstSpec extends FlatSpec {
  "The BST" should "return its size recursively" in {
    val bst = new Bst[String, Int]

    assert(bst.size === 0)

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)

    assert(bst.size === 3)
  }

  it should "be able to find inserted keys (order them correctly)" in {
    val bst = new Bst[String, Int]

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)
    bst.insert("qux", -3)
    bst.insert("quux", 12)
    bst.insert("corge", 5)

    assert(bst.find("foo") === Some(8))
    assert(bst.find("bar") === Some(2))
    assert(bst.find("baz") === Some(6))
    assert(bst.find("qux") === Some(-3))
    assert(bst.find("quux") === Some(12))
    assert(bst.find("corge") === Some(5))
  }

  it should "return None if key is not found" in {
    val bst = new Bst[String, Int]

    assert(bst.find("foo") === None)
  }

  it should "do nothing if deleting non-existing key" in {
    val bst = new Bst[String, Int]

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)
    assert(bst.size === 3)

    bst.delete("foobar")
    assert(bst.size === 3)
  }

  it should "remove the key from the tree after deleting" in {
    val bst = new Bst[String, Int]

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)
    bst.insert("qux", -3)
    bst.insert("quux", 12)
    bst.insert("corge", 5)

    bst.delete("foo")
    assert(bst.find("foo") === None)
  }

  it should "be able to find the minimum and maximum value" in {
    val bst = new Bst[String, Int]

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)
    bst.insert("qux", -3)
    bst.insert("quux", 12)
    bst.insert("corge", 5)

    assert(bst.max === Some(-3)) // qux
    assert(bst.min === Some(2)) // bar
  }

  it should "be able to return the correct in-order traversal values" in {
    val bst = new Bst[String, Int]

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)
    bst.insert("qux", -3)
    bst.insert("quux", 12)
    bst.insert("corge", 5)

    val expectedInorder = "(bar, 2), (baz, 6), (corge, 5), (foo, 8), (quux, 12), (qux, -3)"

    assert(bst.inorderString === expectedInorder)
  }

  it should "return the same in-order traversal string no matter what order the values are inserted" in {
    val bst = new Bst[String, Int]

    bst.insert("qux", -3)
    bst.insert("bar", 2)
    bst.insert("corge", 5)
    bst.insert("foo", 8)
    bst.insert("baz", 6)
    bst.insert("quux", 12)

    val expectedInorder = "(bar, 2), (baz, 6), (corge, 5), (foo, 8), (quux, 12), (qux, -3)"

    assert(bst.inorderString === expectedInorder)
  }

  it should "be able to return the correct in-order traversal values after deleting a key" in {
    val bst = new Bst[String, Int]

    bst.insert("foo", 8)
    bst.insert("bar", 2)
    bst.insert("baz", 6)
    bst.insert("qux", -3)
    bst.insert("quux", 12)
    bst.insert("corge", 5)

    bst.delete("foo")

    val expectedInorder = "(bar, 2), (baz, 6), (corge, 5), (quux, 12), (qux, -3)"

    assert(bst.inorderString === expectedInorder)
  }

}
