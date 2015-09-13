package sh.echo.swagged

import scala.reflect.api.Universe

case class RouteContext(method: RouteContext.Method.Value, path: List[RouteContext.Segment])

object RouteContext {
  val empty = RouteContext(Method.None, Nil)

  sealed trait Segment
  object Segment {
    case class Fixed(value: String) extends Segment
    case class Param(name: String, dataType: String) extends Segment
  }

  object Method extends Enumeration {
    val None, Get, Post = Value
  }

  def toSwaggerSpec(rcs: List[RouteContext]) = {
    val groupedByPath = rcs.groupBy(_.path)

    def genOperation(rc: RouteContext) = SwaggerSpec.Operation(
      Map("200" → SwaggerSpec.Response("")),
      rc.path collect { case Segment.Param(name, dataType) ⇒
        SwaggerSpec.Parameter(
          name,
          SwaggerSpec.Parameter.In.Path,
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

trait RouteContextLiftable {
  val universe: Universe
  import universe._

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
}
