package u03

import org.junit.*
import org.junit.Assert.*
import Lists.*
import u02.AlgebraicDataTypes.Person
import Task2.*

class Task2Test:
    import u02.AlgebraicDataTypes.Person.* 
    import List.*

    val p1 = Teacher("mario", "story")
    val p2 = Teacher("giovanni", "gymnastic")
    val l : List[Person] = Cons(p1, Cons(p2, Nil()))

    @Test def getCourses() = 
        // println(l)
        assertEquals(List.Cons("story", Cons("gymnastic", Nil())), courses(l))
