package exercise1

/** Напишите отдельные функции, решающие поставленную задачу. 
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
object Functions {

  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */
  val circleArea = (r: Double) => r * r * Math.PI
  
  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = circleArea(r)
  
  
  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  val rectAreaCurried = (a: Double) => (b: Double) => a * b
  
  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = rectAreaCurried(a)(b)
  

  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  val rectArea = (a: Double, b: Double) => a * b

  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = rectArea(a, b)


  def main(args: Array[String]): Unit = {
    print("circleArea(5) = " + circleArea(5));
    print("\nrectAreaCurried(7)(9) = " + rectAreaCurried(7)(9));
    print("\nrectArea(7, 9) = " + rectArea(7, 9));
  }
}
