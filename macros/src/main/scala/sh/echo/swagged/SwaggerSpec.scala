package sh.echo.swagged

import scala.util.Failure
import scala.util.Success
import scala.util.Try

import spray.json._

case class SwaggerSpec(
  swagger: String,
  info: SwaggerSpec.Info,
  paths: Map[String, SwaggerSpec.PathInfo])

object SwaggerSpec {
  import DefaultJsonProtocol._

  object Parameter {
    object In extends Enumeration {
      val query, header, path, formData, body = Value
      implicit val jf = new JsonFormat[Value] {
        def write(v: Value) = JsString(v.toString)
        def read(v: JsValue) = Try(withName(v.asInstanceOf[JsString].value)) match {
          case Success(x) ⇒ x
          case Failure(e) ⇒ deserializationError(s"Expected Parameter.In value, but got [$v].", e)
        }
      }
    }
  }

  case class Info(title: String, version: String)
  case class PathInfo(get: Option[Operation] = None, post: Option[Operation] = None)
  case class Operation(responses: Map[String, Response], parameters: Option[List[Parameter]] = None)
  case class Parameter(name: String, in: Parameter.In.Value, required: Boolean, `type`: String)
  case class Response(description: String)

  implicit val jf1 = jsonFormat1(Response)
  implicit val jf2 = jsonFormat4(Parameter.apply)
  implicit val jf3 = jsonFormat2(Operation)
  implicit val jf4 = jsonFormat2(PathInfo)
  implicit val jf5 = jsonFormat2(Info)
  implicit val jf6 = jsonFormat3(SwaggerSpec.apply)
}
