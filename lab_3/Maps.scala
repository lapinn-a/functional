

/** Напишите вашу реализацию в тестовые функции.
  * 
  * https://docs.scala-lang.org/overviews/collections/maps.html
  */
object Maps {

  case class User(name: String, age: Int)

  /* a) В данной Seq[User] сгруппируйте пользователей по имени (`groupBy`) и вычислите средний возраст: `name -> averageAge`
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testGroupUsers(users: Seq[User]): Map[String, Int] =
    users.
      groupBy(_.name).
      map(x => (x._1, x._2.foldLeft(0)((a, b) => a + b.age / x._2.length)))

  /* b) Дана `Map[String, User]` состоящая из имен пользователей `User`, сколько имен пользователей, содержащихся в Map, содержат подстроку "Adam"?
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testNumberFrodos(map: Map[String, User]): Int = map.count(x => x._2.name.contains("Adam"))

  /* c) Удалите всех пользователей возраст которых менее 35 лет.
   *    Вы можете реализовать ваше решение в теле тестовой функции. Не изменяйте сигнатуру.
   */
  def testUnderaged(map: Map[String, User]): Map[String, User] = map.filter(x => x._2.age >= 35)

  def main(args: Array[String]): Unit = {
    print("testGroupUsers(Seq(User(\"Alice\", 25), User(\"Bob\", 24), User(\"Bob\", 19), User(\"Alice\", 20), User(\"Alice\", 18))) = " +
      testGroupUsers(Seq(User("Alice", 34), User("Bob", 23), User("Bob", 19), User("Alice", 20), User("Alice", 18))))

    print("\n\ntestNumberFrodos(Map(\"1\" -> User(\"Alice\", 34), \"2\" -> User(\"definitely not Adam\", 23), \"3\" -> User(\"Adam\", 19), \"4\" -> User(\"Bob\", 20))) = " +
      testNumberFrodos(Map("1" -> User("Alice", 34), "2" -> User("definitely not Adam", 23), "3" -> User("Adam", 19), "4" -> User("Bob", 20))))

    print("\n\ntestUnderaged(Map(\"1\" -> User(\"Alice\", 44), \"2\" -> User(\"Bob\", 33), \"3\" -> User(\"Alice\", 37), \"4\" -> User(\"Alice\", 28))) = " +
      testUnderaged(Map("1" -> User("Alice", 44), "2" -> User("Bob", 33), "3" -> User("Alice", 37), "4" -> User("Alice", 28))))
  }
}
