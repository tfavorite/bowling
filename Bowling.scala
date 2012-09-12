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

  def getBonus(rolls:List[Char]): Int = rolls.head match {
      case STRIKE => getFrameValue(rolls.tail.head, rolls.tail.tail.head)
      case _ => rolls.tail.head match {
        case SPARE => getThrowValue(rolls.tail.tail.head)
        case _ => 0
      }
  }

  def getTotalScore(scores: List[Char], framesleft: Int = 10):Int = framesleft match {
      case 0 => 0
      case _ => scores.head match {
        case STRIKE => getThrowValue(scores.head) + getBonus(scores) + getTotalScore(scores.tail, framesleft - 1)
        case _ => getFrameValue(scores.head, scores.tail.head) + getBonus(scores) + getTotalScore(scores.tail.tail, framesleft - 1)
      }
  }

  val score = args(0).toUpperCase.toCharArray.toList
  println("Total score: " + getTotalScore(score))

}
