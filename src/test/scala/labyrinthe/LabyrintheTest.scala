package labyrinthe

import munit.ScalaCheckSuite
import org.scalacheck.*

import labyrinthe.*

class LabyrintheTests extends ScalaCheckSuite:

  test("assert True") {
    val obtained = 42
    val expected = 42
    assertEquals(obtained, expected)
  }

  test("assert false") {
    val obtained = 43
    val expected = 42
    assertNotEquals(obtained, expected)
  }
