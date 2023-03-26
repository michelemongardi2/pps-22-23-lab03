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
}
