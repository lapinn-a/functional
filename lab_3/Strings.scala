

/** Напишите ваши решения в тестовых функциях.
  * 
  * https://www.scala-lang.org/api/2.12.3/scala/collection/immutable/StringOps.html
  */
object Strings {

  /* a) Преобразуйте все символы типа Char в верхний регистр (не используйте заглавные буквы).
   *    
   */
  def testUppercase(str: String): String = str.toUpperCase


  /* b) Вставьте следующие значения в строку:
   *       Hi my name is <name> and I am <age> years old.
   *    
   */
  def testInterpolations(name: String, age: Int): String = String.format("Hi my name is %s and I am %d years old.", name, age)


  /* c) Добавьте два числа в следующую строку:
   *       Hi,
   *       now follows a quite hard calculation. We try to add:
   *         a := <value of a>
   *         b := <value of b>
   * 
   *         result is <a + b>
   * 
   *   
   */
  def testComputation(a: Int, b: Int): String = String.format("Hi,\nnow follows a quite hard calculation. We try to add:\n  a := %d\n  b := %d\n\n  result is %d", a, b, a + b)


  /* d) Если длина строки равна 2, верните всю строку, иначе верните первые два символа строки.
   */
  def testTakeTwo(str: String): String = str.length match {
    case 2 => str
    case _ => str.take(2)
  }


  def main(args: Array[String]): Unit = {
    print("testUppercase(\"stop screaming at me\") = " + testUppercase("stop screaming at me"))

    print("\n\ntestInterpolations(\"Victor\", 20) = " + testInterpolations("Victor", 20))

    print("\n\ntestComputation(3, 5) =\n" + testComputation(3, 5))

    print("\n\ntestTakeTwo(\"hi\") = " + testTakeTwo("hi"))
    print("\ntestTakeTwo(\"hello\") = " + testTakeTwo("hello"))
  }
}
