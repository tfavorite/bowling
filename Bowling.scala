object Bowling extends App {

  val STRIKE = 'X'
  val SPARE = '/'

  def getThrowValue(t: Char) = t match {
    case STRIKE => 10
    case _ => t.asDigit
  }

  def getFrameValue(t1: Char, t2: Char) = t2 match {
    case SPARE => 10
    case _ => getThrowValue(t1) + getThrowValue(t2)
  }

  def getBonus(roll:Char, nextrolls:List[Char]): Int = roll match {
    case STRIKE => nextrolls match {
      case Nil => 0
      case head::Nil => 0
      case head::next => getFrameValue(head, next.head)
      case _ => 0
    }
    case SPARE => getThrowValue(nextrolls.head)  // if it's a spare, there must be one more roll
    case _ => 0
  }

  def getTotalScore(scores: List[Char]):Int = scores match {
    case Nil => 0
    case s::ss => s match {
      case STRIKE =>  ss match {
        case Nil => getThrowValue(s)
        case head::Nil => getThrowValue(s)
        case head::next => getThrowValue(s) + getBonus(head, next) + getTotalScore(ss)
      }
      case _ => ss.head match {
        case SPARE => 10 + getBonus(ss.head, ss.tail) + getTotalScore(ss.tail)
        case _ => getFrameValue(s, ss.head) + getTotalScore(ss.tail)
      }
    }
  }

   val scores: List[Char] = List('1', '0', '1', '/', '2', '2', 'X', '3', '3', 'X', '1', '/', '3', '/', 'X', '1', '2')
   println(getTotalScore(scores))
    val scores2: List[Char] = List('X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X', 'X')
  println(getTotalScore(scores2))
}
