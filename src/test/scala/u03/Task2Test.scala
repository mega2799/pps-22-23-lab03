package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.AlgebraicDataTypes.Person
import Task.*

class Task2Test:
  import u02.AlgebraicDataTypes.Person.*
  import List.*

  val p1 = Teacher("mario", "story")
  val p2 = Teacher("giovanni", "gymnastic")
  val l: List[Person] = Cons(p1, Cons(p2, Nil()))

  @Test def getCourses() =
    // println(l)
    assertEquals(List.Cons("story", Cons("gymnastic", Nil())), courses(l))

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  @Test def foldTest() =
    assertEquals(-16, foldLeft(lst)(0)(_ - _)) // -16
    assertEquals(-8, foldRight(lst)(0)(_ - _)) // -16
