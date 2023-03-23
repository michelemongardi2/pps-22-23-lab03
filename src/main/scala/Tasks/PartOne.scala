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

  def append[A](left: List[A], right: List[A]): List[A] = (left, right) match
    case (Nil(), _) => right
    case (Cons(h, t), right) => Cons(h, append(t, right))

  println("***** Task1 - 1b - Append *****")

  val tail = Cons(40, Nil())
  println(append(lst, tail)) // Cons (10 , Cons (20 , Cons (30 , Cons (40 , Nil ()))))

}
