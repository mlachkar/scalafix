package scalafix.tests.util

import scala.meta._

import org.scalatest.funsuite.AnyFunSuite
import scalafix.testkit.DiffAssertions
import scalafix.v1._

class ScalametaStructureSuite extends AnyFunSuite with DiffAssertions {
  test("pretty(t)") {
    val obtained = q"a.b.c.d".structureWidth(1)
    val expected =
      """|Term.Select(
         |  Term.Select(
         |    Term.Select(
         |      Term.Name("a"),
         |      Term.Name("b")
         |    ),
         |    Term.Name("c")
         |  ),
         |  Term.Name("d")
         |)
         |""".stripMargin
    assertNoDiff(obtained, expected)
  }

  test("pretty(t, showFieldNames = true)") {
    val obtained = q"a.b.c.d".structureLabeled(1)
    val expected =
      """|
         |Term.Select(
         |  qual = Term.Select(
         |    qual = Term.Select(
         |      qual = Term.Name("a"),
         |      name = Term.Name("b")
         |    ),
         |    name = Term.Name("c")
         |  ),
         |  name = Term.Name("d")
         |)
         |""".stripMargin
    assertNoDiff(obtained, expected)
  }

  test("option") {
    assertNoDiff(
      q"def foo: A = ???".decltpe.structureWidth(1),
      """|
         |Some(Type.Name("A"))
         |""".stripMargin
    )
  }

  test("list") {
    assertNoDiff(
      // NOTE(olafur): need downcast because List is no longer a Product in 2.13.
      q"foo(a)".args.asInstanceOf[Product].structureWidth(1),
      """|List(
         |  Term.Name("a")
         |)
         |""".stripMargin
    )
  }
}
