package patmat

class HuffmanSuite extends munit.FunSuite:
  import Huffman.*

  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }


  test("weight of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(weight(t1), 5)
  }


  test("chars of a larger tree (10pts)") {
    new TestTrees:
      assertEquals(chars(t2), List('a','b','d'))
  }

  test("string2chars hello world") {
    assertEquals(string2Chars("hello, world"), List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times method on a simple list of chars") {
    assertEquals(times(List('c', 'b', 'a', 'c', 'c', 'a', 'c', 'a')).toSet, List(('c', 4), ('b', 1), ('a', 3)).toSet)
    assertEquals(times(List('a', 'b', 'a')).toSet, List(('b', 1), ('a', 2)).toSet)
    assertEquals(times(List('a', 'b')).toSet, List(('b', 1), ('a', 1)).toSet)
    assertEquals(times(List('a')), List(('a', 1)))

  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('e', 1))), List(Leaf('e',1)))
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1))), List(Leaf('e',1), Leaf('t',2)))
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist1 = List(Leaf('e', 1), Leaf('t', 2))
    assertEquals(combine(leaflist1), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
    val leaflist2 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist2), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
    val leaflist3 = List(Leaf('e', 3), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist3), List(Leaf('x',4)), Fork(Leaf('e',3),Leaf('t',2),List('e', 't'),5))    
  }


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
