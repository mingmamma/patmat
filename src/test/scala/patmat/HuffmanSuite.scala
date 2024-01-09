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
    assertEquals(times(List('c', 'b', 'a', 'c', 'c', 'a', 'c', 'a')).toSet, List(('a', 3), ('b', 1), ('c', 4)).toSet)
    assertEquals(times(List('a', 'b', 'a')).toSet, List(('a', 2), ('b', 1)).toSet)
    assertEquals(times(List('a', 'b')).toSet, List(('a', 1), ('b', 1)).toSet)
    assertEquals(times(List('a','a')).toSet, List(('a', 2)).toSet)
    assertEquals(times(List('a')).toSet, List(('a', 1)).toSet)
    assertEquals(times(List('e','f','g','h','c','d','b','b','b')).toSet,
    List(('b', 3), ('c', 1), ('d', 1), ('e', 1), ('f', 1), ('g', 1), ('h', 1)).toSet)
  }


  test("make ordered leaf list for some frequency table (15pts)") {
    assertEquals(makeOrderedLeafList(List(('e', 1))), List(Leaf('e',1)))
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1))), List(Leaf('e',1), Leaf('t',2)))
    assertEquals(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))), List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
    assertEquals(makeOrderedLeafList(List(('b', 3), ('c', 1), ('d', 1), ('e', 1), ('f', 1), ('g', 1), ('h', 1))),
    List(Leaf('c', 1),Leaf('d', 1),Leaf('e', 1),Leaf('f', 1),Leaf('g', 1),Leaf('h', 1),Leaf('b', 3)))
  }


  test("combine of some leaf list (15pts)") {
    val leaflist1 = List(Leaf('e', 1), Leaf('t', 2))
    assertEquals(combine(leaflist1), List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))
    
    val leaflist2 = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assertEquals(combine(leaflist2), List(Fork(Leaf('e',1), Leaf('t',2), List('e', 't'),3), Leaf('x', 4)))
    
    val leaflist3 = List(Leaf('t', 2), Leaf('e', 3), Leaf('x', 4))
    assertEquals(combine(leaflist3), List(Leaf('x',4), Fork(Leaf('t',2), Leaf('e',3), List('t', 'e'), 5)))

    val leaflist4 = List(Leaf('c', 1),Leaf('d', 1),Leaf('e', 1),Leaf('f', 1),Leaf('g', 1),Leaf('h', 1),Leaf('b', 3))
    assertEquals(combine(leaflist4), List(Leaf('e', 1),Leaf('f', 1),Leaf('g', 1),Leaf('h', 1),Fork(Leaf('c', 1),Leaf('d', 1), List('c','d'), 2),Leaf('b', 3)))
  }

  test("create a single codeTree from combining leaf lists") {
    // leaflist1
    assertEquals(createCodeTree(List('t', 't', 'e')), Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3))
    
    // leaflist2
    assertEquals(createCodeTree(List('t','x','e','x','x','x','t')), Fork(Fork(Leaf('e',1), Leaf('t',2), List('e', 't'), 3), Leaf('x', 4), List('e', 't', 'x'), 7))
  
    // leaflist3
    assertEquals(createCodeTree(List('t','x','e','x','e','x','x','e','t')), Fork(Leaf('x',4), Fork(Leaf('t',2), Leaf('e',3), List('t', 'e'), 5), List('x', 't', 'e'), 9))

    assertEquals(createCodeTree(List('a','a','a','a','a','a','a','a')),
    Leaf('a', 8))

    assertEquals(createCodeTree(List('e','f','g','h','c','d','b','b','b')),
    Fork(
      Fork(Fork(Leaf('c',1),Leaf('d',1),List('c','d'),2), Fork(Leaf('e',1),Leaf('f',1),List('e','f'),2),List('c','d','e','f'),4),
      Fork(Fork(Leaf('g',1), Leaf('h',1), List('g','h'),2), Leaf('b',3),List('g','h','b'),5),
      List('c','d','e','f','g','h','b'),
      9))

    // reproduce the example Huffman tree
    // https://www.coursera.org/learn/scala-functional-programming/programming/89qyl/huffman-coding/instructions
    assertEquals(createCodeTree(List('a','a','a','a','a','a','a','a','e','f','g','h','c','d','b','b','b')), 
    Fork(
      Leaf('a', 8),
      Fork(
      Fork(Fork(Leaf('c',1),Leaf('d',1),List('c','d'),2), Fork(Leaf('e',1),Leaf('f',1),List('e','f'),2),List('c','d','e','f'),4),
      Fork(Fork(Leaf('g',1), Leaf('h',1), List('g','h'),2), Leaf('b',3),List('g','h','b'),5),
      List('c','d','e','f','g','h','b'),
      9),
      List('a','c','d','e','f','g','h','b'),
      17))
  }

  test("Decode of a short list with two letters") {
    new TestTrees:
      val codedMessage = List(0,1,1,0)
      assertEquals(decode(t1, codedMessage), List('a','b','b','a'))
  }

  test("Enode a short list with two letters") {
    new TestTrees:
      val originalMessage = List('a','b','b','a')
      assertEquals(encode(t1)(originalMessage), List(0,1,1,0))
  }

  test("Enode a short list with QuickEncode") {
    new TestTrees:
      val originalMessage = List('a','b','b','a')
      assertEquals(quickEncode(t1)(originalMessage), List(0,1,1,0))
  }

  test("codeBits should work") {
    new TestTrees:
      assertEquals(codeBits(convert(t1))('a'), List(0))
  }

  test("codeBits should work 2") {
    new TestTrees:
      assertEquals(codeBits(convert(t1))('b'), List(1))
  }  


  test("decode and encode a very short text should be identity (10pts)") {
    new TestTrees:
      assertEquals(decode(t1, encode(t1)("ab".toList)), "ab".toList)
  }

  test("decode and encode a rather long text should be identity") {
    new TestTrees:
      assertEquals(decode(t2, encode(t2)("abdddbba".toList)), "abdddbba".toList)
  }

  test("decode and encode a rather long text with QuickEncode") {
    new TestTrees:
      assertEquals(decode(t2, encode(t2)("abdbabdbdabbdab".toList)), "abdbabdbdabbdab".toList)
  }  

  test("convert a simple CodeTree to CodeTable") {
    new TestTrees:
      assertEquals(convert(t1), List(('a',List(0)),('b',List(1))))
  }

  test("convert another simple CodeTree to CodeTable") {
    new TestTrees:
      assertEquals(convert(t2), List(('a',List(0,0)),('b',List(0,1)),('d',List(1))))
  }  


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
