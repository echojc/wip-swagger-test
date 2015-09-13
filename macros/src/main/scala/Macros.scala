import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.annotation.tailrec
import spray.routing._
import spray.json.DefaultJsonProtocol
import spray.json.NullOptions
import org.scalamacros.resetallattrs._

@compileTimeOnly("enable macro paradise to expand macro annotations")
class swagged extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro swaggedMacro.impl
}

object swaggedMacro {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    object Directive {
      def unapply(t: Tree): Option[(Tree, Tree)] = t match {
        case q"routing.this.Directive.pimpApply[$_]($directive)($_).apply(..$inner)" ⇒
          if (inner.size > 1)
            c.warning(t.pos, s"pimpApply takes 1 argument but found more [$inner]")
          Some((directive, inner.head))
        case _ ⇒
          None
      }
    }

    object Complete {
      def unapply(t: Tree): Boolean = t match {
        case q"$lhs.apply(..$_)" ⇒
          lhs.symbol.fullName == "spray.routing.directives.RouteDirectives.complete"
        case _ ⇒
          false
      }
    }

    def resolveSegment(t: Tree, params: List[String]): (List[RouteContext.Segment], List[String]) =
      t match {
        case Apply(_, List(Literal(Constant(path: String)))) if t.symbol.fullName == "spray.routing.ImplicitPathMatcherConstruction.segmentStringToPathMatcher" ⇒
          (List(RouteContext.Segment.Fixed(path)), params)
        case t if t.tpe <:< typeOf[spray.routing.PathMatcher[_]] ⇒
          val paramTypes = t.tpe.baseType(typeOf[spray.routing.PathMatcher[_]].typeSymbol).typeArgs.head

          @tailrec
          def loop(hlist: Type, params: List[String], out: List[RouteContext.Segment]): (List[RouteContext.Segment], List[String]) =
            hlist.typeArgs match {
              case List(tpe, hlist) if tpe =:= typeOf[Int] ⇒
                val param :: rest = params
                val segment = RouteContext.Segment.Param(param, "integer")
                loop(hlist, rest, segment :: out)
              case List(tpe, hlist) if tpe =:= typeOf[String] ⇒
                val param :: rest = params
                val segment = RouteContext.Segment.Param(param, "string")
                loop(hlist, rest, segment :: out)
              case List(tpe, hlist) ⇒
                c.warning(t.pos, s"unknown pathmatcher type [$tpe]")
                loop(hlist, params, out)
              case _ ⇒
                (out, params)
            }
          loop(paramTypes, params, Nil)
        case _ ⇒
          c.warning(t.pos, s"unknown pathmatcher [$t]")
          (Nil, params)
      }

    def resolvePath(t: Tree, params: List[String]): List[RouteContext.Segment] = {
      @tailrec
      def loop(path: Tree, params: List[String], out: List[RouteContext.Segment]): List[RouteContext.Segment] =
        path match {
          case q"$lhs./[$t]($rhs)($_)" ⇒
            val (segments, leftovers) = resolveSegment(rhs, params)
            loop(lhs, leftovers, segments ++ out)
          case segment ⇒
            val (segments, leftover) = resolveSegment(segment, params)
            if (leftover.nonEmpty)
              c.warning(t.pos, s"unmatched params in pathmatcher!")
            segments ++ out
        }
      // params are resolved back to front
      loop(t, params.reverse, Nil)
    }

    def resolveDirective(t: Tree, params: List[String]): RouteContext ⇒ List[RouteContext] =
      t.symbol.fullName match {
        case "spray.routing.directives.MethodDirectives.get" ⇒
          (r: RouteContext) ⇒ List(r.copy(method = RouteContext.Method.Get))
        case "spray.routing.directives.PathDirectives.path" ⇒
          val Apply(_, paths) = t
          (r: RouteContext) ⇒ paths flatMap (path ⇒ List(r.copy(path = r.path ++ resolvePath(path, params))))
        case _ ⇒
          c.warning(t.pos, s"unknown directive [$t]")
          List(_)
      }

    def parseRoute(t: Tree): RouteContext ⇒ List[RouteContext] =
      t match {
        case Directive(directive, body) ⇒
          body match {
            case q"(..$params) ⇒ $inner" ⇒ resolveDirective(directive, params map (_.name.decodedName.toString)) andThen (_ flatMap parseRoute(inner))
            case inner                   ⇒ resolveDirective(directive, Nil) andThen (_ flatMap parseRoute(inner))
          }
        case Complete() ⇒
          List(_)
        case _ ⇒
          c.warning(t.pos, s"unknown route structure [$t]")
          List(_)
      }

    object OptimusPrime extends Transformer {
      implicit val lift3 = Liftable[RouteContext.Method] {
        case RouteContext.Method.Get ⇒
          q"RouteContext.Method.Get"
        case RouteContext.Method.Post ⇒
          q"RouteContext.Method.Post"
        case RouteContext.Method.None ⇒
          q"RouteContext.Method.None"
      }
      implicit val lift2 = Liftable[RouteContext.Segment] {
        case RouteContext.Segment.Param(n, d) ⇒
          q"RouteContext.Segment.Param($n, $d)"
        case RouteContext.Segment.Fixed(v) ⇒
          q"RouteContext.Segment.Fixed($v)"
      }
      implicit val lift1 = Liftable[RouteContext] { rc ⇒
        q"RouteContext(${rc.method}, ${rc.path})"
      }

      override def transform(t: Tree): Tree = t match {
        case t: ValDef if t.symbol.info =:= typeOf[spray.routing.Route] ⇒
          val result = super.transform(t)
          val ValDef(a1, a2, a3, rhs) = result
          val routeCtxs = parseRoute(rhs)(RouteContext.empty)

          // TODO: no method means all?
          val (methodCtxs, noMethodCtxs) = routeCtxs partition (_.method != RouteContext.Method.None)
          if (!noMethodCtxs.isEmpty)
            c.warning(t.pos, s"found routes without http method [$noMethodCtxs]")

          val transformedRoute = q"""
            (get & path("swagger.json")) {
              complete {
                RouteContext.toSwaggerSpec($methodCtxs)
              }
            } ~ $rhs
          """
          ValDef(a1, a2, a3, transformedRoute)
        case t ⇒
          super.transform(t)
      }
    }

    val outputs = for {
      annottee ← annottees
    } yield {
      val typed = c.typecheck(annottee.tree)
      val transformed = OptimusPrime.transform(typed)
      c.resetAllAttrs(transformed)
    }

    c.Expr[Any] { q"""
      ..${outputs}
    """ }
  }
}

object RouteContext {
  val empty = RouteContext(Method.None, Nil)

  sealed trait Segment
  object Segment {
    case class Fixed(value: String) extends Segment
    case class Param(name: String, dataType: String) extends Segment
  }

  sealed trait Method
  object Method {
    case object None extends Method
    case object Get extends Method
    case object Post extends Method
  }

  def toSwaggerSpec(rcs: List[RouteContext]) = {
    val groupedByPath = rcs.groupBy(_.path)

    def genOperation(rc: RouteContext) = SwaggerSpec.Operation(
      Map("200" → SwaggerSpec.Response("")),
      rc.path collect { case Segment.Param(name, dataType) ⇒
        SwaggerSpec.Parameter(
          name,
          "path",
          true,
          dataType
        )
      } match {
        case Nil ⇒ None
        case list ⇒ Some(list)
      }
    )

    SwaggerSpec(
      "2.0",
      SwaggerSpec.Info("wip", "0"),
      groupedByPath.map { case (path, rcs) ⇒
        val normalisedSegments = path map {
          case Segment.Fixed(value)   ⇒ value
          case Segment.Param(name, _) ⇒ s"{$name}"
        }
        val pathStr = s"/${normalisedSegments.mkString("/")}"
        val get = rcs.find(_.method == Method.Get) map genOperation
        val post = rcs.find(_.method == Method.Post) map genOperation
        pathStr → SwaggerSpec.PathInfo(get, post)
      }.toMap
    )
  }
}
case class RouteContext(method: RouteContext.Method, path: List[RouteContext.Segment])

object SwaggerSpec extends DefaultJsonProtocol {
  case class Info(title: String, version: String)
  case class PathInfo(get: Option[Operation] = None, post: Option[Operation] = None)
  case class Operation(responses: Map[String, Response], parameters: Option[List[Parameter]] = None)
  // todo: `in` should be enum
  case class Parameter(name: String, in: String, required: Boolean, `type`: String)
  case class Response(description: String)

  implicit val jf1 = jsonFormat1(Response)
  implicit val jf2 = jsonFormat4(Parameter)
  implicit val jf3 = jsonFormat2(Operation)
  implicit val jf4 = jsonFormat2(PathInfo)
  implicit val jf5 = jsonFormat2(Info)
  implicit val jf6 = jsonFormat3(SwaggerSpec.apply)
}
case class SwaggerSpec(
  swagger: String,
  info: SwaggerSpec.Info,
  paths: Map[String, SwaggerSpec.PathInfo]
)
