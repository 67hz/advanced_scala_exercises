package exercises

import scala.annotation.tailrec

// covariant A - sub B and C of A can be included in list of A type
abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend op
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] // concat op // by-name to avoid SO

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]


  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A]

}

object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def foreach(f: Nothing => Unit): Unit = () // () == Unit
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this


  def #::[B >: Nothing](element: B): MyStream[B] = new Cons(element, this) // prepend op
  def ++[B >: Nothing](anotherStream: => MyStream[B]): MyStream[B] = anotherStream

  def take(n: Int): MyStream[Nothing] = this
  def takeAsList(n: Int): List[Nothing] = Nil
}

// class because objects above cannot have ctors like h,t unless case with overrides
class Cons[+A](h: A, t: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  // combining call by name with lazy-val is call-by-need
  override val head: A = h // override as val for reuse throughout body
  override lazy val tail: MyStream[A] = t

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  def map[B](f: A => B): MyStream[B] = new Cons(f(head), tail.map(f)) // tail.map(f) is lazily eval'd
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f) // ++ preserves lazy eval -> tail.flatMap(f) is lazy eval'd
  def filter(predicate: A => Boolean): MyStream[A] = {
    if(!predicate(head)) tail.filter(predicate) // first element in tail evald, tail of tail preserves lazy eval
    else new Cons(head, tail.filter(predicate)) // tail.filter(predicate) lazy eval'd
  }

  // non-unique
  /*
    val s = new Cons(1, EmptyStream)
    val prepended = 1 #:: s = new Cons(1, s) // s is lazily evaluated
   */
  def #::[B >: A](element: B): MyStream[B] = new Cons(element, this) // prepend op
  def ++[B >: A](anotherStream: => MyStream[B]): MyStream[B] = { // LAZY Eval otherwise SO error
    //    head #:: tail ++ anotherStream // SO Error
    new Cons(head, tail ++ anotherStream) // this is faster, because (..., tail ++ anotherStream) is lazily eval'd
  }

  // take first n elements of stream
  def take(n: Int): MyStream[A] = n match {
    case 0 => EmptyStream
    case 1 => new Cons(head, EmptyStream) // optimization
    case _ => new Cons(head, tail.take(n - 1)) // tail.take(n-1) lazy eval'd
  }

  def takeAsList(n: Int): List[A] = n match {
    case 0 => List()
    case _ => head :: tail.takeAsList(n - 1)
  }

}


object MyStream {
  def from[A](start: A)(generator: A => A): MyStream[A] = {
    lazy val tHead: A = generator(start) // lazy necessary?
    lazy val tTail: MyStream[A] = from(tHead)(generator)
    new Cons(start, tTail)
  }
}

object MyStreamTest extends App {
  val allRNumbers = MyStream.from(1)(_ + 1)
  val aStream = MyStream.from(1)(x => x + 2)
  val bStream = MyStream.from(2)(x => x + 2)
  val cStream = MyStream.from(1)(x => x * 3)
  val dStream = MyStream.from(1)(x => x * 2)
  val zStream = 999 #:: aStream // aStream#::(999) // prepend 999 bc right-associative
  val comboStream = aStream ++ bStream
//  println(zStream.tail.tail.tail.tail.tail.head)
//  println(zStream.isEmpty)
//  println(zStream.take(400).tail.tail.tail.head)
//    zStream.takeAsList(4000).foreach(println)
//  zStream.take(4).foreach(println)
//  comboStream.take(40).foreach(println)
//  comboStream.take(40).map(x => x * 10).foreach(println)

//  comboStream.take(400).flatMap(x => new Cons(x, new Cons(x * 10, EmptyStream))).foreach(println)
//  comboStream take 8000 flatMap (x => new Cons(x * 10, new Cons(x + 1, EmptyStream))) foreach println
//  println(allRNumbers flatMap(x => new Cons(x, new Cons(x + 1, EmptyStream))) takeAsList 10)

//  val myList = comboStream take 300 flatMap(x => new Cons(x * 10, new Cons(x + 1, EmptyStream))) takeAsList 10
//  println(myList)

//  allRNumbers take 4000 filter(_ % 2 == 0) foreach println
//  allRNumbers take(10000) foreach println


//  lazy val bStream = MyStream.from(2)(x => x * 2)
//  lazy val bothStreams = aStream ++ bStream
////  println(bothStreams.take(2))
//  bothStreams.takeAsList(4).foreach(println)

//  val emptyStream = EmptyStream
//  println(emptyStream.head)
//  emptyStream.takeAsList(3).foreach(println)

  /*
   Exercises on Streams - HARD
   - stream of Fibonacci numbers
   - stream of prime numbers with Eratosthenes' sieve
      [2 3 4 ...
      filter out all numbers div by 2
      [2 3 5 7 9 11 ...]
      filter out all numbers div by 3
      [2 5 7 11 13 17 ...]
      filter out all numbers div by 5 ( next number available in series )
   */

  def fib(i: BigInt): BigInt = {
    @tailrec
    def fibHelper(j: BigInt, prev: BigInt, acc: BigInt): BigInt = {
      if (i == j) acc
      else if (i <= 2) 1
      else fibHelper(j + 1, acc, acc + prev)
    }
    fibHelper(1, 0, 1)
  }

  def fibonacciStream(first: BigInt, second: BigInt): MyStream[BigInt] = {
    new Cons(first, fibonacciStream(second, first + second)) // new Cons(first, _) _ is lazy eval'd
  }

  val fibNumbers = allRNumbers.map(x => fib(x))
  println(fibonacciStream(1, 1).takeAsList(1000))

  def erato(stream: => MyStream[Int]): MyStream[Int] = {
    if (stream.isEmpty) stream
    else {
      val p = stream.head // start at 2
      lazy val subStream = stream.tail.filter(x => x % p != 0)
      new Cons(p, erato(subStream))
    }
  }

  println("\nEratosthenes' sieve")
  val eratoStream = MyStream.from(2)(_ + 1)
  println(erato(eratoStream).takeAsList(4000))




}
