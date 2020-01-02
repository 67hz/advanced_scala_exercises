package exercises

// covariant A - sub B and C of A can be included in list of A type
abstract class MyStream[+A] {
  def isEmpty: Boolean

  def head: A
  def tail: MyStream[A]

  def #::[B >: A](element: B): MyStream[B] // prepend op
  def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] // concat op

  def foreach(f: A => Unit): Unit
  def map[B](f: A => B): MyStream[B]
  def flatMap[B](f: A => MyStream[B]): MyStream[B]
  def filter(predicate: A => Boolean): MyStream[A]


  def take(n: Int): MyStream[A]
  def takeAsList(n: Int): List[A]

}

case object EmptyStream extends MyStream[Nothing] {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException
  def tail: MyStream[Nothing] = throw new NoSuchElementException

  def foreach(f: Nothing => Unit): Unit = ()
  def map[B](f: Nothing => B): MyStream[B] = this
  def flatMap[B](f: Nothing => MyStream[B]): MyStream[B] = this
  def filter(predicate: Nothing => Boolean): MyStream[Nothing] = this


  def #::[B >: Nothing](element: B): MyStream[B] = new NonEmptyStream[B](element, EmptyStream) // prepend op
  def ++[B >: Nothing](anotherStream: MyStream[B]): MyStream[B] = anotherStream

  def take(n: Int): MyStream[Nothing] = this
  def takeAsList(n: Int): List[Nothing] = List()
}

// class because objects above cannot have ctors like h,t unless case with overrides
class NonEmptyStream[+A](h: => A, t: => MyStream[A]) extends MyStream[A] {
  def isEmpty: Boolean = false

  def head: A = h
  def tail: MyStream[A] = t

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail.foreach(f)
  }
  def map[B](f: A => B): MyStream[B] = f(head) #:: tail.map(f)
  def flatMap[B](f: A => MyStream[B]): MyStream[B] = f(head) ++ tail.flatMap(f)
  def filter(predicate: A => Boolean): MyStream[A] = {
    if(!predicate(head)) tail.filter(predicate)
    else head #:: tail.filter(predicate)
  }

  // non-unique
  def #::[B >: A](element: B): MyStream[B] = new NonEmptyStream[B](element, this) // prepend op
  def ++[B >: A](anotherStream: MyStream[B]): MyStream[B] = {
    //    head #:: tail ++ anotherStream // SO Error
    new NonEmptyStream(head, tail ++ anotherStream) // this is faster, why?
  }

  // take first n elements of stream
  def take(n: Int): MyStream[A] = n match {
    case 0 => EmptyStream
    case _ => head #:: tail.take(n - 1)
  }

  def takeAsList(n: Int): List[A] = n match {
    case 0 => List()
    case _ => head :: tail.takeAsList(n - 1)
  }

}


object MyStream {
  def from[A](start: => A)(generator: A => A): MyStream[A] = {
    lazy val tHead: A = generator(start) // lazy necessary?
    lazy val tTail: MyStream[A] = from(tHead)(generator)
    new NonEmptyStream(start, tTail)
//    start #:: tTail // SO Error

  }
}

object MyStreamTest extends App {
  val aStream = MyStream.from(1)(x => x + 2)
  val bStream = MyStream.from(2)(x => x + 2)
  val cStream = MyStream.from(1)(x => x * 3)
  val dStream = MyStream.from(1)(x => x * 2)
  val zStream = 999 #:: aStream
  val comboStream = aStream ++ bStream
//  println(zStream.tail.tail.tail.tail.tail.head)
//  println(zStream.isEmpty)
//  println(zStream.take(400).tail.tail.tail.head)
//    zStream.takeAsList(4000).foreach(println)
//  zStream.take(4).foreach(println)
//  comboStream.take(40).foreach(println)
//  comboStream.take(40).map(x => x * 10).foreach(println)

//  comboStream.take(400).flatMap(x => new NonEmptyStream(x, new NonEmptyStream(x * 10, EmptyStream))).foreach(println)
//  comboStream take 400 flatMap (x => new NonEmptyStream(x * 10, new NonEmptyStream(x + 1, EmptyStream))) foreach println
  dStream take 400 filter(_ % 2 == 0) foreach println


//  lazy val bStream = MyStream.from(2)(x => x * 2)
//  lazy val bothStreams = aStream ++ bStream
////  println(bothStreams.take(2))
//  bothStreams.takeAsList(4).foreach(println)

//  val emptyStream = EmptyStream
//  println(emptyStream.head)
//  emptyStream.takeAsList(3).foreach(println)


}
