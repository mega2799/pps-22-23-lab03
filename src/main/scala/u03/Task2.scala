package u03

import u02.AlgebraicDataTypes.Person
import u03.Lists.*
import u03.Lists.List.*
import u03.Lists.List.flatMap
import u03.Lists.List.map
import u03.Lists.List.filter

object Task2 extends App:
  def courses(l : List[Person]) : List[String] = 
    flatMap(l)( p => p match
        case Person.Teacher(name, course) => List.Cons(course, Nil())
        case _ => Nil()
    ) 