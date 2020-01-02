package exercises

import scala.annotation.tailrec

// a collection that extends a function that is a function
trait MySet[A] extends (A => Boolean) {

  // apply required because of extending (A => Boolean)
  def apply(element: A): Boolean =
    contains(element)

  def contains(elem: A): Boolean
  def +(elem: A): MySet[A]
  def ++(anotherSet: MySet[A]): MySet[A] // Union
  def map[B](f: A => B): MySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B]
  def filter(predicate: A => Boolean): MySet[A]
  def foreach(f: A => Unit): Unit

  /*
    - remove an element
    - intersection with another set
    - difference with another set
   */

  // space required between ! and : otherwise : will be included in method name
  def unary_! : MySet[A]
  def -(elem: A): MySet[A]
  def &(anotherSet: MySet[A]): MySet[A]
  def --(anotherSet: MySet[A]): MySet[A] // difference
}

// class instead of singleton object because of invariance
class EmptySet[A] extends MySet[A] {
  def contains(elem: A): Boolean = false
  def +(elem: A):MySet[A] = new NonEmptySet[A](elem, this)
  def ++(anotherSet: MySet[A]): MySet[A] = anotherSet
  def map[B](f: A => B): MySet[B] = new EmptySet[B]
  def flatMap[B](f: A => MySet[B]): MySet[B] = new EmptySet[B]
  def filter(predicate: A => Boolean): MySet[A] = this
  def foreach(f: A => Unit): Unit = () // () = Unit

  def -(elem: A): MySet[A] = this
  def &(anotherSet: MySet[A]): MySet[A] = this
  def --(anotherSet: MySet[A]): MySet[A] = this

  // negate an empty set means a full set of all elements of type A
  def unary_! = new PropertyBasedSet[A](_ => true)
}

// contains all values of type A - problematic so use property based where property is A => Boolean
// would run into issues implementing map, flatMap, filter, forEach and -
//class AllInclusiveSet[A] extends MySet[A]

// all elements of A that satisfy where property() is true
// this is a POTENTIALLY INFINITE SET
// { x in A | property(x) } // math def
class PropertyBasedSet[A](property: A => Boolean) extends MySet[A] {
  override def contains(elem: A): Boolean = property(elem)

  // { x in A | property(x) } + element = { x in A | property(x) || x == element }
  // all x in A where property(x) is true or any x where x == elem because this is a property based set + any non-property-based element
  override def +(elem: A): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || x == elem)

  // { x in A | property(x) } ++ set = { x in A | property(x) || set contains x }
  override def ++(anotherSet: MySet[A]): MySet[A] =
    new PropertyBasedSet[A](x => property(x) || anotherSet(x))


  // naturals = AllInclusiveSet[Int] = all natural numbers
  // naturals.map(x => x % 3) => ???
  // [0 1 2] // infinite to finite set
  // TRICKY: If you map a function to an infinite set you cannot know if result is finite or not so cannot tell if
  // element is in set or not
  override def map[B](f: A => B): MySet[B] = politelyFail
  override def flatMap[B](f: A => MySet[B]): MySet[B] = politelyFail
  override def foreach(f: A => Unit): Unit = politelyFail

  // all values of type A that satisfy predicate - could be infinite
  // hold only elements where both property and filter predicate hold true
  override def filter(predicate: A => Boolean): MySet[A] = new PropertyBasedSet[A](x => property(x) && predicate(x))

  override def -(elem: A): MySet[A] = filter(x => x != elem)
  override def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)
  override def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  // negation of all is empty
  override def unary_! : MySet[A] = new PropertyBasedSet[A](x => !property(x))

  def politelyFail = throw new IllegalArgumentException("Really deep rabbit hole")

}

class NonEmptySet[A](head: A, tail: MySet[A]) extends MySet[A] {
  def contains(elem: A): Boolean =
    elem == head || tail.contains(elem)

  // + adds unique only
  def +(elem: A): MySet[A] = {
    if (this.contains(elem)) this
    else new NonEmptySet[A](elem, this)
  }

  /*
    [1 2 3] ++ [4 5]
    [2 3] ++ [4 5] + 1
    [3] ++ [4 5] ++ 1 + 2
    [] ++ [4 5] ++ 1 + 2 + 3 // NES+
    [4 5] + 1 + 2 + 3 = [4 5 1 2 3] // ES++
   */
  // recursively call ++ on tail and then add head
  def ++(anotherSet: MySet[A]): MySet[A] = tail ++ anotherSet + head

  def map[B](f: A => B): MySet[B] = (tail map f) + f(head)

  def flatMap[B](f: A => MySet[B]): MySet[B] = (tail flatMap f) ++ f(head)

  def filter(predicate: A => Boolean): MySet[A] = {
    val filteredTail = tail filter predicate
    if (predicate(head)) filteredTail + head
    else filteredTail
  }

  def foreach(f: A => Unit): Unit = {
    f(head)
    tail foreach f
  }

  def -(elem: A): MySet[A] = {
    if (head == elem) tail
    else tail - elem + head
  }

  // contains is part of contract () from trait so anotherSet(elem) == anotherSet.contains(elem)
  // filter(elem => anotherSet(elem)  == filter(anotherSet)
  def &(anotherSet: MySet[A]): MySet[A] = filter(anotherSet)

  def --(anotherSet: MySet[A]): MySet[A] = filter(!anotherSet)

  // to negate a set
  // infinity minus set => (x => !this.contains(x))
  def unary_! : MySet[A] = new PropertyBasedSet[A](x => !this(x))

}

object MySet {
  /*
    val s = MySet(1,2,3) = buildSet(seq(1,2,3), [])
    = buildSet(seq(2,3), [] + 1)
    = buildSet(seq(3), [1] + 2
    = buildSet(seq(), [1,2] + 3)
    = isEmpty -> [1,2,3]
   */
  // A* = multiple values of A
  def apply[A](values: A*): MySet[A] = {
    @tailrec
    def buildSet(valSeq: Seq[A], acc: MySet[A]): MySet[A] =
      if (valSeq.isEmpty) acc
      else buildSet(valSeq.tail, acc + valSeq.head)

    // convert values to sequence because starts at vararg collection
    buildSet(values.toSeq, new EmptySet[A])
  }
}

object MySetPlayground extends App {

  val s = MySet(1,2,3,4)

//  s + 5 ++ MySet(-1, -2) + 3 foreach println
//  s + 5 ++ MySet(-1, -2) + 3 map (x => x * 10) foreach println
//  s + 5 ++ MySet(-1, -2) + 3 flatMap (x => MySet(x, 10 * x)) filter (_ % 2 == 0) foreach println

  val mySet1 = MySet(1,22,44)
  val mySet2 = MySet(232, 22, 44, 44, 44)
//  mySet1 - 22 foreach println

//  mySet1 & mySet2 foreach println
//  mySet1 -- mySet2 foreach println

  val negative = !mySet1 // s.unary_! => all naturals != (1,2,3,4)
//  println(negative(22))
//  println(negative(5))

  val negativeEven = negative.filter(_ % 2 == 0)
  println(negativeEven(5))
  val negativeEven5 = negativeEven + 5
  println(negativeEven5(5))

  val setA = !mySet1
  val setB = MySet(2,3,5,898)



}



