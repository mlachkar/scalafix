package scalafix.tests.config

import metaconfig.Conf
import metaconfig.typesafeconfig._
import scalafix.internal.config.ScalafixConfig

class ScalafixConfigSuite extends munit.FunSuite {
  def check(name: String, config: String, expected: ScalafixConfig)(
      implicit loc: munit.Location
  ): Unit = {
    test(name) {
      val obtained = Conf.parseString(config).get.as[ScalafixConfig].get
      assertEquals(obtained, expected)
    }
  }
  check(
    "version",
    "version = abba",
    ScalafixConfig(version = "abba")
  )
}
