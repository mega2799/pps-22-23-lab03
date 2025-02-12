package u03

import math.Fractional.Implicits.infixFractionalOps
  import math.Integral.Implicits.infixIntegralOps
  import math.Numeric.Implicits.infixNumericOps
import u03.Streams.Stream.{constante, fibs}

object Streams extends App :

  import Lists.*

  enum Stream[A]:
    private case Empty()
    private case Cons(head: () => A, tail: () => Stream[A])

  object Stream:

    def empty[A](): Stream[A] = Empty()

    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] =
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)

    def toList[A](stream: Stream[A]): List[A] = stream match
      case Cons(h, t) => List.Cons(h(), toList(t()))
      case _ => List.Nil()

    def map[A, B](stream: Stream[A])(f: A => B): Stream[B] = stream match
      case Cons(head, tail) => cons(f(head()), map(tail())(f))
      case _ => Empty()

    def filter[A](stream: Stream[A])(pred: A => Boolean): Stream[A] = stream match
      case Cons(head, tail) if (pred(head())) => cons(head(), filter(tail())(pred))
      case Cons(head, tail) => filter(tail())(pred)
      case _ => Empty()

    def take[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => cons(head(), take(tail())(n - 1))
      case _ => Empty()

    def drop[A](stream: Stream[A])(n: Int): Stream[A] = (stream, n) match
      case (Cons(head, tail), n) if n > 0 => drop(tail())(n - 1)
      case (Cons(head, tail), n)  => cons(head(), tail())
      case _ => Empty()

    def iterate[A](init: => A)(next: A => A): Stream[A] =
      cons(init, iterate(next(init))(next))

    def constante[A](k: A): Stream[A] = (k) match
      case _ => Stream.cons(k, constante(k))

    def fibonacci(n : Int) : Int = n match
      case 0 => 0
      case 1 => 1 
      case _ => fibonacci(n-1) + fibonacci(n-2)
     
    def fibs: Stream[Int] =
      Stream.map(iterate(fibonacci(0))(_ + 1))(fibonacci(_))
      
  end Stream


// var simplifies chaining of functions a bit..
//  var str = Stream.iterate(0)(_ + 1) // {0,1,2,3,..}
//  str = Stream.map(str)(_ + 1) // {1,2,3,4,..}
//  str = Stream.filter(str)(x => (x < 3 || x > 20)) // {1,2,21,22,..}
//  str = Stream.take(str)(10) // {1,2,21,22,..,28}
//  println(Stream.toList(str)) // [1,2,21,22,..,28]
//
//  val corec: Stream[Int] = Stream.cons(1, corec) // {1,1,1,..}
//  println(Stream.toList(Stream.take(corec)(10))) // [1,1,..,1]

    val s = Stream.take(Stream.iterate(0)(_ + 1))(10)
    println{Stream.toList(Stream.drop(s)(6))}
    println(Stream.toList(Stream.take(constante("x"))(5)))
    println(Stream . toList ( Stream . take ( fibs ) (8) ))


