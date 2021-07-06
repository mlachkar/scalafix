/*
rules = [
  "Scala3NewSyntax"
]
*/
package scala3NewSyntax

object VarargsSyntax {
  def splices(): Unit = {
    val xs = List(1, 2, 3)
    List(xs: _*)
    List((xs ++ List(5, 6)): _*)
    List((xs ++ List(xs: _*)): _*)
    List(xs*) // no-op
  }

  def patterns(): Unit = {
    val xs = List(1, 2, 3)
    xs match {
      case List(ys @ _*) => ()
    }
    xs match {
      case List(_ @ _*) => ()
    }
    xs match {
      case List(ys*) => () // no-op
    }
    xs match {
      case List(_*) => () // no-op
    }
  }
}
