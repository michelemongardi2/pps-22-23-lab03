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

  val lst = Cons(10, Cons(20, Cons(30, Nil())))
  println(drop(lst, 1)) // Cons (20 , Cons (30 , Nil ()))
  println(drop(lst, 2)) // Cons (30 , Nil ())
  println(drop(lst, 5)) // Nil ()

}
