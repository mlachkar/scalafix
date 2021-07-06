
package scala3NewSyntax

object VarargsSyntax {
  def splices(): Unit = {
    val xs = List(1, 2, 3)
    List(xs*)
    List((xs ++ List(5, 6))*)
    List((xs ++ List(xs*))*)
    List(xs*) // no-op
  }

  def patterns(): Unit = {
    val xs = List(1, 2, 3)
    xs match {
      case List(ys*) => ()
    }
    xs match {
      case List(_*) => ()
    }
    xs match {
      case List(ys*) => () // no-op
    }
    xs match {
      case List(_*) => () // no-op
    }
  }
}
