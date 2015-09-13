import akka.actor._
import spray.httpx._
import spray.routing._
import sh.echo.swagged.swagged

@swagged trait Abc extends HttpService with SprayJsonSupport {
  val route =
    get {
      path("pets" / IntNumber / "name" / Segment) { (petId, name) ⇒
        complete {
          "pets!"//s"pet id=[$petId] name=[$name]"
        }
      }
    }
}

object Main extends App with SimpleRoutingApp with Abc {
  implicit val system = ActorSystem()

  startServer(interface = "0.0.0.0", port = 8080) {
    route
  }
}
