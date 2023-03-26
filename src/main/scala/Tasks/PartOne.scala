package Tasks

import scala.annotation.tailrec

object PartOne extends App {

  import u03.Lists.*
  import List.*

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = (l, n) match
    case (Nil(), _) => Nil()
    case (Cons(_, t), 1) => t
    case (Cons(_, t), n) => drop(t, n-1)

  println("***** Task1 - 1a - Drop *****")

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  println(drop(lst, 1)) // Cons (20 , Cons (30 , Nil ()))
  println(drop(lst, 2)) // Cons (30 , Nil ())
  println(drop(lst, 5)) // Nil ()

  def append[A](left: List[A], right: List[A]): List[A] = left match
    case Nil() => right
    case Cons(h, t) => Cons(h, append(t, right))

  println()
  println("***** Task1 - 1b - Append *****")

  val tail = Cons(40, Nil())
  println(append(lst, tail)) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ()))))

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] = l match
    case Nil() => Nil()
    case Cons(h, t) => append(f(h), flatMap(t)(f))

  println()
  println("***** Task1 - 1c - FlatMap *****")
  println(flatMap(lst)(v => Cons(v + 1, Nil()))) // Cons (11 , Cons (21 , Cons (31 , Nil ())))
  println(flatMap(lst)(v => Cons(v + 1, Cons(v + 2, Nil())))) // Cons (11 , Cons (12 , Cons (21 , Cons (22 , Cons (31 , Cons (32 , Nil ()))))))

  def newMap[A, B](l: List[A])(mapper: A => B): List[B] = l match
    case Nil() => Nil()
    case _ => flatMap(l)(a => Cons(mapper(a), Nil()))

  println()
  println("***** Task1 - 1d - Map *****")
  println(newMap(lst)(x => x+2)) // Cons (12 , Cons (22 , Cons (32 , Nil ())))

  def newFilter[A](l: List[A])(predicate: A => Boolean): List[A] = flatMap(l)(predicateToCons(predicate))
  def predicateToCons[A](pred: A => Boolean): (A => List[A]) = a => if(pred(a)) Cons(a, Nil()) else Nil()

  println()
  println("***** Task1 - 1d - Filter *****")
  val list = Cons(10, Cons(20, Cons(9, Cons(30, Cons(1, Cons(5, Nil()))))))
  println(newFilter(list)(x => x > 10)) // Cons (20 , Cons (30 , Nil ())))

  /**
   * Task 2
   */

  import u02.Optionals.*
  import Option.*

  def max(l: List[Int]): Option[Int] = findMax(l, None())
  @tailrec
  def findMax(l: List[Int], maxOpt: Option[Int]): Option[Int] = (l, maxOpt) match
    case (Nil(), _) => maxOpt
    case (Cons(h, t), None()) => findMax(t, Some(h))
    case (Cons(h, t), Some(a)) =>
      val newMax = if(a < h) Some(h) else maxOpt
      findMax(t, newMax)

  println()
  println("***** Task2 - Max *****")
  println(max(Cons(10, Cons(25, Cons(20, Nil()))))) // Some (25)
  println(max(Nil())) // None ()

}
