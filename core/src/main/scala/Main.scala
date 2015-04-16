import akka.actor._
import spray.routing._

trait Abc extends HttpService {

  @swagger
  val route =
    get {
      path("pets") {
        identity("some side effect")
        complete {
          "all pets"
        }
      } ~
      path("pets" / IntNumber / "foods" / IntNumber) { (petId, foodId) ⇒
        complete {
          s"just pet $petId with food $foodId"
        }
      } ~
      path("owners") {
        complete {
          "all owners"
        }
      }
    } ~
    post {
      path("pets") {
        complete {
          "posted all pets!"
        }
      }
    }
}

object Main extends App with SimpleRoutingApp with Abc {
  implicit val system = ActorSystem()

  startServer(interface = "localhost", port = 8080) {
    route
  }
}
