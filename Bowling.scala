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
      case head::next => {
        val bonus = getFrameValue(head, next.head)
        println("Strike bonus: " + bonus)
        bonus
      }
      case _ => 0
    }
    case SPARE => {
      val bonus = getThrowValue(nextrolls.head)
      println("Spare bonus: " + bonus)
      bonus
    }  // if it's a spare, there must be one more roll
    case _ => 0
  }

  def getTotalScore(scores: List[Char]):Int = {
    println(scores)
    scores match {
      case Nil => 0
      case head::Nil => 0
      case head::tail => {
        val bonus = {
          head match {
            case STRIKE => getBonus(head, tail)
            case _ => {
              tail.head match {
                case SPARE => getBonus(tail.head, tail.tail)
                case _ => 0
              }
            }
          }
        }
        head match {
          case STRIKE => {
            println(10 + bonus)
            10 + bonus + getTotalScore(tail)
          }
          case _ => {
            val frameValue = getFrameValue(head, tail.head)
            println(frameValue + bonus)
            frameValue + bonus + getTotalScore(tail.tail)
          }
        }
      }
    }
  }

  def validateScore( scoreString: String ): Option[List[Char]] = {
    val bowlRegex = """(X|\d(\d|/)){9}(X(X(X|\d)|\d(\d|/))|\d(\d|(/|(X|\d))))""".r
    bowlRegex.findFirstIn(scoreString.toUpperCase).map(_.toCharArray.toList)
  }

  val score:Option[List[Char]] = validateScore(args(0))
  score.flatMap((s:List[Char]) => println("Total score: " + getTotalScore(s))).getOrElse(println("Invalid score, try again."))
  score match {
    case None => println("Invalid score, try again.")
    case Some(s) => println("Total score: " + getTotalScore(s))
  }

}
