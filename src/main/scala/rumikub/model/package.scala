package rumikub

import scala.annotation.tailrec

package object model {
  def removeFirstMatch[T](item: T, list: List[T]): List[T] = {
    @tailrec
    def recurse(remaining: List[T], accumulated: List[T]): List[T] =
      remaining match {
        case Nil => accumulated.reverse
        case head :: tail =>
          if (head == item) accumulated.reverse ::: tail
          else recurse(tail, head :: accumulated)
      }
      
    recurse(list, Nil)
  }
}
