package sh.echo

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

package object swagged {
  @compileTimeOnly("enable macro paradise to expand macro annotations")
  class swagged extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro SwaggedMacro.impl
  }
}
