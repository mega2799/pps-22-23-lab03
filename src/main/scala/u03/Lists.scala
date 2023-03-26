package u03

import scala.collection.immutable.LazyList.cons

object Lists extends App :

  // A generic linkedlist
  enum List[E]:
    case Cons(head: E, tail: List[E])
    case Nil()
  // a companion object (i.e., module) for List
  object List:
    
    def sum(l: List[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _ => 0

    def map[A, B](l: List[A])(mapper: A => B): List[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil() => Nil()

    def filter[A](l1: List[A])(pred: A => Boolean): List[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t) => filter(t)(pred)
      case Nil() => Nil()

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
      // case l => flatMap(l)(el => filter(l)(el > _)) match
      //   case Nil() => None
      //   case Cons(h, t) => Some(h)
      



  val l = List.Cons(10, List.Cons(20, List.Cons(30, List.Nil())))
  println(List.sum(l)) // 60

  import List.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  println(max(l))
