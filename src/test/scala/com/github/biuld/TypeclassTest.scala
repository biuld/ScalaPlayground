package com.github.biuld

import org.junit.jupiter.api.{Assertions, DisplayName, Test}
import typeclass.Monoid

class TypeclassTest {

  /**
   * Say we need a sum func for both the Int type and String type
   * Now we achieve it by using the type class Monoid
   * Furthermore, we can look into the alternative approaches like inheritance and the adaptor-pattern for better understanding
   *
   * Ref:
   * https://docs.scala-lang.org/tour/implicit-parameters.html
   * https://www.youtube.com/watch?v=1e9tcymPl7w
   */
  @DisplayName("Type Class")
  @Test
  def test(): Unit = {
    val intXs: List[Int] = (for (x <- 0 to 10) yield x).toList

    val stringXs: List[String] = (for (x <- "Hello, world!".split("")) yield x).toList

    Assertions.assertEquals(Monoid.sum(intXs), intXs.sum)
    Assertions.assertEquals(Monoid.sum(stringXs), "Hello, world!")
  }
}
