
import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
  * 
  * Seq(1, 2) match {
  *   case head +: tail => ???
  *   case Nil          => ???
  *   case s            => ???
  * }
  * 
  * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
  */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *    
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = {
    @tailrec
    def loop(seq: Seq[A]): Option[A] = seq match {
      case Nil => None
      case Seq(a) => Some(a)
      case _ :: tail => loop(tail)
    }
    loop(seq)
  }


  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *    
   */
  def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop(a: Seq[A], b: Seq[A], acc: Seq[(A, A)]): Seq[(A, A)] = a match {
      case h1 :: Nil => b match {
        case Nil => acc
        case h2 :: _ => acc :+ (h1, h2)
      }
      case h1 :: t1 => b match {
        case Nil => acc
        case h2 :: t2 => loop(t1, t2, acc :+ (h1, h2))
      }
      case Nil => acc
    }
    loop(a, b, Nil)
  }


  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *    
   */
  def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop(seq: Seq[A], acc: Boolean): Boolean = seq match {
      case head :: tail => loop(tail, acc && cond(head))
      case Nil => acc
    }
    loop(seq, true)
  }


  /* d) Проверьте, является ли Seq палиндромом
   *    
   */
  def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop(s: Seq[A], acc: Seq[A]): Boolean = s match {
      case head :: tail => loop(tail, head +: acc)
      case Nil => seq.equals(acc)
    }
    loop(seq, Nil)
  }


  /* e) Реализуйте flatMap используя foldLeft.
   *    
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((x, y) => x ++: f(y))


  def main(args: Array[String]): Unit = {
    print("testLastElement(Seq(1, 2, 3, 4, 5)) = " + testLastElement(Seq(1, 2, 3, 4, 5)))
    print("\ntestLastElement(Nil) = " + testLastElement(Nil))

    print("\n\ntestZip(Seq(1, 2), Seq(3, 4)) = " + testZip(Seq(1, 2), Seq(3, 4)))
    print("\ntestZip(Seq(1, 2), Seq(3, 4, 5)) = " + testZip(Seq(1, 2), Seq(3, 4, 5)))
    print("\ntestZip(Seq(3, 4, 5), Nil) = " + testZip(Seq(3, 4, 5), Nil))

    print("\n\ntestForAll(Seq(3, 4, 5))((x: Int) => x < 6) = " + testForAll(Seq(3, 4, 5))((x: Int) => x < 6))
    print("\ntestForAll(Seq(3, 4, 9))((x: Int) => x < 6) = " + testForAll(Seq(3, 4, 9))((x: Int) => x < 6))
    print("\ntestForAll(Nil)((x: Int) => x < 6) = " + testForAll(Nil)((x: Int) => x < 6))

    print("\n\ntestPalindrom(Seq(1, 2, 3, 2, 1)) = " + testPalindrom(Seq(1, 2, 3, 2, 1)))
    print("\ntestPalindrom(Seq(1, 2, 3, 2, 1, 0)) = " + testPalindrom(Seq(1, 2, 3, 2, 1, 0)))

    print("\n\ntestFlatMap(Seq(1, 2, 3, 4, 5))((x: Int) => Seq(x, 2 * x)) = " + testFlatMap(Seq(1, 2, 3, 4, 5))((x: Int) => Seq(x, 2 * x)))
  }
}
