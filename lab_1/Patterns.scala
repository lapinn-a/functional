package exercise1

/** Напишите решение в виде функции. 
  * 
  * Синтаксис:
  *   val a: Int = ???
  * 
  *   a match {
  *     case 0 => true
  *     case _ => false
  *   }
  */
object PatternMatching {

  sealed trait Hand
  case object Rock    extends Hand
  case object Paper   extends Hand
  case object Scissor extends Hand

  sealed trait Result
  case object Win  extends Result
  case object Lose extends Result
  case object Draw extends Result

  sealed trait Food
  case object Meat       extends Food
  case object Vegetables extends Food
  case object Plants     extends Food

  sealed trait Animal {

    val name: String
    var food: Food
  }
  case class Mammal(name: String, var food: Food, weight: Int) extends Animal
  case class Fish(name: String, var food: Food)                extends Animal
  case class Bird(name: String, var food: Food)                extends Animal


  /* a) Напишите функцию, которая ставит в соответствие числу строку слудеющим образом:
   * Если:
   *     1 => "it is one"
   *     2 => "it is two"
   *     3 => "it is three"
   *     иначе => "what's that"
   */
   def toString(value: Int): String = value match {
     case 1 => "it is one"
     case 2 => "it is two"
     case 3 => "it is three"
     case _ => "what's that"
   }

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testIntToString(value: Int): String = toString(value)


  /* b) Напишите функцию которая возвращает true если переменная `value` принимает значение:
   *     "max" или "Max
   *     "moritz" или "Moritz"
   */
  def matchValue(value: String): Boolean = value match {
    case "max" | "Max" | "moritz" | "Moritz" => true
    case _ => false
  }

  // примените функции из пункта (b) здесь, не изменяя сигнатуру
  def testIsMaxAndMoritz(value: String): Boolean = matchValue(value)


  // c) Напишите функцию проверки является ли `value` четным
  def isEven(value: Int): Boolean = value % 2 match {
    case 0 => true
    case 1 => false
  }

  // примените функции из пункта (c) здесь, не изменяя сигнатуру
  def testIsEven(value: Int): Boolean = isEven(value)

  
  /* d) Напишите функцию, моделирующую игру в Камень ножницы бумага 
   *     1. камень побеждает ножницы
   *     2. ножницы побеждают бумагу
   *     3. бумага побеждает камень
   *    Выиграет ли игрок `a`?
   */
  def winsA(a: Hand, b: Hand): Result = (a, b) match {
    case (Rock, Scissor) | (Scissor, Paper) | (Paper, Rock) => Win
    case (Rock, Rock) | (Scissor, Scissor) | (Paper, Paper) => Draw
    case _ => Lose
  }

  // примените вашу функцию из пункта (d) здесь, не изменяя сигнатуру
  def testWinsA(a: Hand, b: Hand): Result = winsA(a, b)


  // Примечание: используйте определение Animals
  // e) Верните вес (weight: Int) объекта Mammal, иначе верните -1.
  def extractMammalWeight(animal: Animal): Int = animal match {
    case animal : Mammal => animal.weight
    case _ => -1
  }

  // примените функцию из пункта (e) здесь, не изменяйте сигнатуру
  def testExtractMammalWeight(animal: Animal): Int = extractMammalWeight(animal)


  // f) Измените поле еда объектов классов Fishes и Birds на Plants, класс Mammels оставьте неизмененным.
  def updateFood(animal: Animal): Animal = animal match {
    case animal : Mammal => animal
    case _ => animal.food = Plants; animal
  }

  // примените функцию из пункта (f) здесь, не изменяйте сигнатуру
  def testUpdateFood(animal: Animal): Animal = updateFood(animal)


  def main(args: Array[String]): Unit = {
    print("toString(1) = " + toString(1))
    print("\ntoString(2) = " + toString(2))
    print("\ntoString(3) = " + toString(3))
    print("\ntoString(4) = " + toString(4))

    print("\n\nmatchValue(\"Max\") = " + matchValue("Max"))
    print("\nmatchValue(\"moritz\") = " + matchValue("moritz"))
    print("\nmatchValue(\"Max Moritz\") = " + matchValue("Max Moritz"))

    print("\n\nisEven(1) = " + isEven(1))
    print("\nisEven(2) = " + isEven(2))

    print("\n\nwinsA(Scissor, Paper) = " + winsA(Scissor, Paper))
    print("\nwinsA(Scissor, Scissor) = " + winsA(Scissor, Scissor))
    print("\nwinsA(Paper, Scissor) = " + winsA(Paper, Scissor))

    val animal1 = Mammal("cat", Meat, 3)
    val animal2 = Bird("parrot", Vegetables)
    print("\n\nanimal1 = " + animal1)
    print("\nanimal2 = " + animal2)
    print("\n\nextractMammalWeight(animal1) = " + extractMammalWeight(animal1))
    print("\nextractMammalWeight(animal2) = " + extractMammalWeight(animal2))

    updateFood(animal1)
    updateFood(animal2)

    print("\n\nanimal1 = " + animal1)
    print("\nanimal2 = " + animal2)
  }
}
