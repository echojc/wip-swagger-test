package sh.echo.swagged

import scala.language.experimental.macros
import scala.annotation.tailrec
import scala.reflect.macros.whitebox.Context

import org.scalamacros.resetallattrs._
import spray.routing._

object SwaggedMacro {
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
          val q"$_(..$paths)" = t
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
      // TODO get rid of these
      implicit val lift3 = Liftable[RouteContext.Method.Value] {
        case RouteContext.Method.Get ⇒
          q"_root_.sh.echo.swagged.RouteContext.Method.Get"
        case RouteContext.Method.Post ⇒
          q"_root_.sh.echo.swagged.RouteContext.Method.Post"
        case RouteContext.Method.None ⇒
          q"_root_.sh.echo.swagged.RouteContext.Method.None"
      }
      implicit val lift2 = Liftable[RouteContext.Segment] {
        case RouteContext.Segment.Param(n, d) ⇒
          q"_root_.sh.echo.swagged.RouteContext.Segment.Param($n, $d)"
        case RouteContext.Segment.Fixed(v) ⇒
          q"_root_.sh.echo.swagged.RouteContext.Segment.Fixed($v)"
      }
      implicit val lift1 = Liftable[RouteContext] { rc ⇒
        q"_root_.sh.echo.swagged.RouteContext(${rc.method}, ${rc.path})"
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
                sh.echo.swagged.RouteContext.toSwaggerSpec($methodCtxs)
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
