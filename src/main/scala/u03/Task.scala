package u03

import u02.AlgebraicDataTypes.Person
import u03.Lists.*
import u03.Lists.List.*
import u03.Lists.List.map
import u03.Lists.List.filter

object Task extends App:
  //Task 1 

    def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Nil(), _) => Nil()
    case (l, 0) => l
    case (Cons(h,t), nItems) => drop(t, nItems -1)
    
    def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
      case (Nil(), right) => right // edge cases
      case (left, Nil()) => left   // edge cases
      case (Cons(h, Nil()), right) => Cons(h, right)
      case (Cons(h, t), right) => Cons(h, append(t, right))
    
    def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
      case Nil() => Nil()
      case Cons(h, t) => append(f(h),flatMap(t)(f))

    // Write map & filter in terms of flatMap 

    def mapf[A, B](l: List[A])(f: A => B): List[B] = l match
      case Cons(h, t) => flatMap(l)((a) => Cons(f(a), Nil()))
      case Nil() => Nil()


    def filterFlat[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => flatMap(l1)((h) => Cons(h,  Nil()))
      case Cons(_, t) => filterFlat(t)(pred)
      case Nil() => Nil()

    def max(l: List[Int]): Option[Int] = l match
      case Nil() => None
      case Cons(h, Nil()) => Some(h)
      case Cons(h,t) => Some(Math.max(h, max(t).head))

    // Task 2 
    def courses(l : List[Person]) : List[String] = 
      flatMap(l)( p => p match
          case Person.Teacher(name, course) => List.Cons(course, Nil())
          case _ => Nil()
      ) 
    
    def foldLeft(l : List[Int])(start: Int)(pred: (Int, Int) => Int) : Int = (l, start) match
      case (Cons(h,t), start) => foldLeft(t)(pred(start, h))(pred)
      case (Nil(), start) => start
    
    def foldRight(l : List[Int])(start: Int)(pred: (Int, Int) => Int) : Int = (l, start) match
      case (Cons(h,t), start) if t != Nil() => pred(h, foldRight(t)(start)(pred))
      case (Cons(h,_), start) => pred(h, start)
      