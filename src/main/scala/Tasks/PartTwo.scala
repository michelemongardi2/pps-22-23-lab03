package Tasks

object PartTwo extends App {

  import u03.Lists.*
  import List.*
  import u02.Modules.*
  import Person.*
  import PartOne.newFilter
  import PartOne.newMap
  import PartOne.flatMap

  def getCourses(l: List[Person]): List[String] = l match
    case Nil() => Nil()
    case _ => newMap(newFilter(l)(el => el match
      case Teacher(_,_) => true
      case _ => false))(tcr => tcr match
        case Teacher(_, course) => course)

  val profs: List[Person] = Cons(Teacher("Sarullo", "Machine Learning"), Cons(Teacher("Cantoni", "Applicazioni e Servizi Web"), Cons(Student("Michele", 2), Cons(Teacher("Petruzzi", "Accountability"), Nil()))))

  println()
  println("***** Task Part2 - getCourse *****")
  println(profs)
  println(getCourses(profs)) //Cons(Machine Learning, Cons(Applicazioni e servizi web, Cons(Accountability, Nil())))

  def betterGetCourses(l: List[Person]): List[String] = flatMap(l)(_ match
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    )

  println()
  println("***** Task Part2 - betterGetCourse *****")
  println(betterGetCourses(profs)) //Cons(Machine Learning, Cons(Applicazioni e servizi web, Cons(Accountability, Nil())))

  def foldLeft(l: List[Int])(default: Int)(f: (Int, Int) => Int): Int = l match
    case Cons(h, t) => foldLeft(t)(f(default, h))(f)
    case Nil() => default

  def foldRight(l: List[Int])(default: Int)(f: (Int, Int) => Int): Int = l match
    case Cons(h, t) => f(h, foldRight(t)(default)(f))
    case Nil() => default

  val list = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  println()
  println("***** Task Part2 - foldLeft *****")
  println(foldLeft(list)(0)(_ - _)) // -16
  println(foldLeft(Nil())(0)(_ + _)) // 0
  println()
  println("***** Task Part2 - foldRight *****")
  println(foldRight(list)(0)(_ - _)) // -8 --> (3-(7-(1-(5-0))))
  println(foldRight(list)(0)(_ + _)) // 16 --> (3+(7+(1+(5+0))))
  println(foldRight(Nil())(0)(_ + _)) // 0


}
