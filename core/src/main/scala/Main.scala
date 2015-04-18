import akka.actor._
import spray.routing._

@swagger
trait Abc extends HttpService {
  val route =
    get {
      path("pets") {
        complete {
          "pets!"// $petId!"
        }
      }
    }
  //val route =
  //  get {
  //    path("pets") {
  //      identity("some side effect")
  //      complete {
  //        "all pets"
  //      }
  //    } ~
  //    path("pets" / IntNumber / "foods" / IntNumber) { (petId, foodId) ⇒
  //      complete {
  //        s"just pet $petId with food $foodId"
  //      }
  //    } ~
  //    path("owners" / Segment) { name ⇒
  //      complete {
  //        s"owner $name"
  //      }
  //    }
  //  } ~
  //  post {
  //    path("pets") {
  //      complete {
  //        "posted all pets!"
  //      }
  //    }
  //  }
}

object Main extends App with SimpleRoutingApp with Abc {
  implicit val system = ActorSystem()

  startServer(interface = "localhost", port = 8080) {
    route
  }
}
