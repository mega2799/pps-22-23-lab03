package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import Task.* 

class ListTest:
  import List.*

  val l: List[Int] = Cons(10, Cons(20, Cons(30, Nil())))

  @Test def testSum() =
    assertEquals(0, sum(Nil()))
    assertEquals(60, sum(l))

  @Test def testMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), map(l)(_ + 1))
    assertEquals(Cons("10", Cons("20", Cons("30", Nil()))), map(l)(_ + ""))

  
  @Test def testFilter() =
    assertEquals(Cons(20, Cons(30, Nil())), filter(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filter(l)(_ != 20))

  @Test def testDrop() =
    assertEquals(Cons(20, Cons(30, Nil())), drop(l, 1))
    assertEquals(Cons(30, Nil()), drop(l, 2))
    assertEquals(Nil(), drop(l, 5))

  val tail = Cons(40, Nil())

  @Test def testAppend() =
    assertEquals(Cons(10, Cons(20, Cons(30, Cons(40, Nil())))), append(l, tail))

  @Test def testAppendEdge() =
    assertEquals(tail, append(Nil(), tail))
    assertEquals(tail, append(tail, Nil()))

  @Test def testFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), flatMap(l)(v => Cons(v + 1, Nil()))) //
    assertEquals(
      Cons(11, Cons(12, Cons(21, Cons(22, Cons(31, Cons(32, Nil())))))),
      flatMap(l)(v => Cons(v + 1, Cons(v + 2, Nil())))
    )

  @Test def testMapLikeFlatMap() =
    assertEquals(Cons(11, Cons(21, Cons(31, Nil()))), mapf(l)(_ + 1))

  @Test def testFilterLikeFlatMap() = // non chiaro perche non funzioni 
    assertEquals(Cons(20, Cons(30, Nil())), filterFlat(l)(_ >= 20))
    assertEquals(Cons(10, Cons(30, Nil())), filterFlat(l)(_ != 20))

  @Test def testMax() =
    assertEquals(Some(25), max(Cons(10, Cons(25, Cons(20, Nil())))))
    assertEquals(Some(30), max(List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))))
    assertEquals(None, max(Nil()))
