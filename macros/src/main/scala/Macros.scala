import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly

@compileTimeOnly("enable macro paradise to expand macro annotations")
class swagger extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro swaggerMacro.impl
}

object swaggerMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def parsePathMatcher(t: Tree, names: List[String]): List[String] = {
      def foo(tree: Tree): List[Tree] = {
        tree match {
          case q"$lhs / $rhs" ⇒
            foo(lhs) :+ rhs
          case t ⇒
            List(t)
        }
      }

      def merge(ts: List[Tree], names: List[String]): List[String] = {
        ts match {
          case Literal(Constant(str: String)) :: rest ⇒
            str :: merge(rest, names)
          case q"IntNumber" :: rest ⇒
            s"{${names.head}}" :: merge(rest, names.tail)
          case Nil ⇒
            Nil
          case _ ⇒
            c.abort(c.enclosingPosition, s"unknown path matcher: [${showRaw(t)}]")
        }
      }

      merge(foo(t), names)
    }

    def loop(t: Tree, rc: RouteContext, out: List[RouteContext]): List[RouteContext] = {
      t match {
        case apply: Apply ⇒
          val fun = apply.fun
          val args = apply.args
          //println(s"# [$fun] [$args]")
          fun match {
            case q"get" ⇒
              args flatMap { arg ⇒
                loop(arg, rc.copy(method = "get"), out)
              }
            case q"post" ⇒
              args flatMap { arg ⇒
                loop(arg, rc.copy(method = "post"), out)
              }
            case q"path($pathMatcher)" ⇒
              args flatMap { arg ⇒
                val names: List[String] = arg match {
                  case q"(..$params) ⇒ $expr" ⇒ (params map (_.name.decodedName.toString)).toList
                  case _                      ⇒ Nil
                }
                loop(arg, rc.copy(path = parsePathMatcher(pathMatcher, names)), out)
              }
            case q"complete" ⇒
              rc :: out
            case q"$expr.~" ⇒ // or'd routes
              (expr :: args) flatMap { arg ⇒
                loop(arg, rc, out)
              }
            case t ⇒
              c.abort(c.enclosingPosition, s"unknown directive: [${showRaw(t)}]")
          }
        case q"(..$params) ⇒ $expr" ⇒ // deconstruct functions
          loop(expr, rc, out)
        case q"{ ..$exprs }" ⇒ // deconstruct blocks - MUST come after functions
          loop(exprs.last, rc, out)
        case t ⇒
          c.abort(c.enclosingPosition, s"unknown route definition: [${showRaw(t)}]")
      }
    }

    implicit val lift = Liftable[RouteContext] { rc ⇒
      q"swaggerMacro.RouteContext(${rc.method}, ${rc.path})"
    }

    val inputs = annottees map (_.tree)
    val outputs = inputs map {
      case q"val $route = $body" ⇒
        //println(showRaw(body))
        val rcs = loop(body, RouteContext.empty, Nil)
        q"""
          val $route =
            (get & path("swagger")) {
              complete { ${rcs.toList}.toString }
            } ~
            $body
        """
      case _ ⇒
        c.abort(c.enclosingPosition, "vals only")
    }

    c.Expr[Any] { q"""
      ..${outputs}
    """ }
  }

  object RouteContext {
    val empty = RouteContext("", Nil)
  }
  case class RouteContext(method: String, path: List[String])

}
