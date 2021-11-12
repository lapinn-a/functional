package Lab_ex4

import scala.annotation.tailrec

object Typeclasses {

  // a) Определите тайп-класс Reversable, который представляет в обратном порядке значения.
  trait Reversable[T] {
    def reverse(a: T): T
  }


  // b) Реализуйте функцию Reverse для String.
  object Reversable {
    implicit object ReversableString extends Reversable[String] {
      def reverse(a: String): String = {
        @tailrec
        def loop(s: String, acc: String): String = s.length match {
          case 0 => acc
          case _ => loop(s.drop(1), s(0) + acc)
        }
        loop(a, "")
      }
    }
  }

  def reverse[A](a: A)(implicit reversable: Reversable[A]): A = reversable.reverse(a)

  // примените тайп-класс-решение из пункта (a) здесь
  def testReversableString(str: String): String = reverse(str)


  // c) Определите тайп-класс Smash таким образом чтобы в нем была функция smash, которая выполняет операцию со значениями одного типа.
  trait Smash[T] {
    def smash(x: T, y: T): T
  }

  object Smash {
    // d) Реализуйте  функции Smash для типа Int и Double.
    //    Используйте сложение для типа Int у умножение для типа Double.
    implicit object SmashInt extends Smash[Int] {
      def smash(x: Int, y: Int): Int = x + y
    }

    implicit object SmashDouble extends Smash[Double] {
      def smash(x: Double, y: Double): Double = x * y
    }

    // e) Реализуйте функцию Smash для типа String. Необходимо выполнить конкатенацию строк, которые будут получены в качестве параметра.
    implicit object SmashString extends Smash[String] {
      def smash(x: String, y: String): String = x concat y
    }
  }

  def smash[A](x: A, y: A)(implicit smash: Smash[A]): A = smash.smash(x, y)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashInt(a: Int, b: Int): Int = smash(a, b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashDouble(a: Double, b: Double): Double = smash(a, b)

  // примените тайп-класс-решение из пункта (d) здесь
  def testSmashString(a: String, b: String): String = smash(a, b)


  // Реализуйте тестовые функции с выводом на экран проверки разработанных функций.
  def main(args: Array[String]): Unit = {
    print("testReversableString(\"12345\") = " + testReversableString("12345"))

    print("\n\ntestSmashInt(30, 50) = " + testSmashInt(30, 50))
    print("\ntestSmashDouble(4.0, 2.5) = " + testSmashDouble(4.0, 2.5))
    print("\ntestSmashString(\"type\", \"Classes\") = " + testSmashString("type", "Classes"))
  }
}
