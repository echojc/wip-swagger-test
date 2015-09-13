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
          SwaggerSpec.Parameter.In.path,
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
