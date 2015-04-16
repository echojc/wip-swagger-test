import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import spray.json.DefaultJsonProtocol
import spray.json.NullOptions

@compileTimeOnly("enable macro paradise to expand macro annotations")
class swagger extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro swaggerMacro.impl
}

object swaggerMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def parsePathMatcher(t: Tree, names: List[String]): PathContext = {
      def convertTreeToList(tree: Tree): List[Tree] = {
        tree match {
          case q"$lhs / $rhs" ⇒
            convertTreeToList(lhs) :+ rhs
          case t ⇒
            List(t)
        }
      }

      def merge(ts: List[Tree], names: List[String]): PathContext = {
        ts match {
          case Literal(Constant(str: String)) :: rest ⇒
            val pc = merge(rest, names)
            PathContext(str :: pc.segments, pc.params)
          case q"IntNumber" :: rest ⇒
            val pc = merge(rest, names.tail)
            PathContext(
              s"{${names.head}}" :: pc.segments,
              ParamContext(names.head, "integer") :: pc.params
            )
          case q"Segment" :: rest ⇒
            val pc = merge(rest, names.tail)
            PathContext(
              s"{${names.head}}" :: pc.segments,
              ParamContext(names.head, "string") :: pc.params
            )
          case Nil ⇒
            PathContext(Nil, Nil)
          case _ ⇒
            c.abort(c.enclosingPosition, s"unknown path matcher: [${showRaw(t)}]")
        }
      }

      merge(convertTreeToList(t), names)
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

    implicit val lift3 = Liftable[ParamContext] { pc ⇒
      q"swaggerMacro.ParamContext(${pc.name}, ${pc.dataType})"
    }
    implicit val lift2 = Liftable[PathContext] { pc ⇒
      q"swaggerMacro.PathContext(${pc.segments}, ${pc.params})"
    }
    implicit val lift1 = Liftable[RouteContext] { rc ⇒
      q"swaggerMacro.RouteContext(${rc.method}, ${rc.path})"
    }

    val inputs = annottees map (_.tree)
    val outputs = inputs map {
      case q"val $route = $body" ⇒
        val rcs = loop(body, RouteContext.empty, Nil)
        q"""
          val $route =
            (get & path("swagger.json")) {
              complete {
                import spray.httpx.SprayJsonSupport._
                swaggerMacro.routeContextsToSwaggerSpec($rcs)
              }
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
    val empty = RouteContext("", PathContext(Nil, Nil))
  }
  case class RouteContext(method: String, path: PathContext)
  case class PathContext(segments: List[String], params: List[ParamContext])
  case class ParamContext(name: String, dataType: String)

  def routeContextsToSwaggerSpec(rcs: List[RouteContext]) = {
    val groupedByPath = rcs.groupBy(_.path.segments)

    def genOperation(rc: RouteContext) = SwaggerSpec.Operation(
      rc.path.params map (param ⇒ SwaggerSpec.Parameter(
        param.name,
        "path",
        true,
        param.dataType
      )) match {
        case Nil ⇒ None
        case list ⇒ Some(list)
      },
      Map("200" → SwaggerSpec.Response(""))
    )

    SwaggerSpec(
      "2.0",
      SwaggerSpec.Info("wip", "0"),
      groupedByPath.map { case (segments, rcs) ⇒
        val path = s"/${segments.mkString("/")}"
        val get = rcs.find(_.method == "get") map genOperation
        val post = rcs.find(_.method == "post") map genOperation
        path → SwaggerSpec.PathInfo(get, post)
      }.toMap
    )
  }
}

// boilerplate yada yada yada
object SwaggerSpec extends DefaultJsonProtocol {
  case class Info(title: String, version: String)
  case class PathInfo(get: Option[Operation], post: Option[Operation])
  case class Operation(parameters: Option[List[Parameter]], responses: Map[String, Response])
  case class Parameter(name: String, in: String, required: Boolean, `type`: String)
  case class Response(description: String)

  implicit val jf1 = jsonFormat1(Response)
  implicit val jf2 = jsonFormat4(Parameter)
  implicit val jf3 = jsonFormat2(Operation)
  implicit val jf4 = jsonFormat2(PathInfo)
  implicit val jf6 = jsonFormat2(Info)
  implicit val jf7 = jsonFormat3(SwaggerSpec.apply)
}
case class SwaggerSpec(
  swagger: String,
  info: SwaggerSpec.Info,
  paths: Map[String, SwaggerSpec.PathInfo]
)
