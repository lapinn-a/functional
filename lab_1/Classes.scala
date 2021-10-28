package exercise1

/* 
 * 
 a) Создать класс Animal, который имеет следующие поля:
 *      - name: String (название)
 *      - species: String (вид)
 *      - food: String
 * 
 *    Синтаксис: class MyClass(val publicField: Int, privateField: String) {
 *              // остальные поля и методы
 *            }
 * 
 * b) Создайте объект-компаньон для класса Animal и добавьте следующие сущности как поля:
 *      - cat, mammal, meat
 *      - parrot, bird, vegetables
 *      - goldfish, fish, plants
 * 
 *    Синтаксис: object MyClass {
 *              // статические поля и методы
 *            }
 * 
 * c) Добавьте следующие метод в Animals:
 *      def eats(food: String): Boolean
 *    
 *     который проверяет ест ли животное определенную пищу
 * 
 * d) Переопределите ваш класс Animal как трейт и создайте объекты класса-образца для Mammals, Birds и Fishs.
 *    Вам все еще нужно поле `species`?
 * 
 * e) Добавьте следующие функции в объект-компаньон Animal:
 *      def knownAnimal(name: String): Boolean  // true если это имя одного из трех животных из (b)
 *      def apply(name: String): Option[Animal] // возвращает одно из трех животных в соответствии с именем (Some) или ничего (None), см. ниже
 * 
 * f) Создайте трейт Food со следующими классами-образцами:
 *      - Meat
 *      - Vegetables
 *      - Plants
 *   и добавьте это в определение Animal. Так же добавьте объект-компаньон с методом apply():
 *      def apply(food: String): Option[Food]
 */

trait Food
case class Meat()       extends Food
case class Vegetables() extends Food
case class Plants()     extends Food

object Food {
  def apply(name: String): Option[Food] = {
    name match {
      case "" => None()
      case "Meat" => Some(Meat())
      case "Vegetables" => Some(Vegetables())
      case "Plants" => Some(Plants())
    }
  }
}

trait Animal {
  val name: String
  val food: Food
  def eats(food: Food): Boolean = this.food == food
}
case class Mammals(name: String, food: Food, weight: Int) extends Animal
case class Birds(name: String, food: Food)                    extends Animal
case class Fishs(name: String, food: Food)                    extends Animal

object Animal {
  def knownAnimal(name: String): Boolean = name match {
    case "cat" | "parrot" | "goldfish" => true
    case _ => false
  }
  def apply(name: String): Option[Animal] = name match {
    case "cat" => Some(Mammals(name, Meat(), 3))
    case "parrot" => Some(Birds(name, Vegetables()))
    case "goldfish" => Some(Fishs(name, Plants()))
    case _ => None()
  }
}

sealed trait Option[A] {
  def isEmpty: Boolean
}
case class Some[A](a: A) extends Option[A] {
  val isEmpty = false
}
case class None[A]()     extends Option[A] {
  val isEmpty = true
}

object Classes{
  def main(args: Array[String]): Unit = {
    val animal1 = Mammals("cat", Meat(), 3)
    val animal2 = Birds("parrot", Vegetables())
    val animal3 = Fishs("goldfish", Plants())
    print("animal1 = " + animal1)
    print("\nanimal2 = " + animal2)
    print("\nanimal2 = " + animal3)

    print("\n\nanimal1.eats(\"Meat()\") = " + animal1.eats(Meat()))
    print("\nanimal1.eats(\"Vegetables()\") = " + animal1.eats(Vegetables()))

    print("\n\nAnimal.knownAnimal(\"cat\") = " + Animal.knownAnimal("cat"))
    print("\nAnimal.knownAnimal(\"mouse\") = " + Animal.knownAnimal("mouse"))

    print("\n\nAnimal.apply(\"cat\") = " + Animal.apply("cat"))
    print("\nAnimal.apply(\"mouse\") = " + Animal.apply("mouse"))
  }
}