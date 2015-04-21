import org.scalatest._
import spray.testkit._
import spray.routing._
import spray.httpx._
import akka.actor._

class Test extends Spec {

  it("simple get route") {
    @swagged trait SimpleGet extends TestRoute {
      val route =
        get {
          complete {
            "got!"
          }
        }
    }

    testRoute(new SimpleGet{}) {
      responseAs[SwaggerSpec] shouldBe SwaggerSpec(
        "2.0",
        SwaggerSpec.Info("wip", "0"),
        Map(
          "/" → SwaggerSpec.PathInfo(
            get = Some(SwaggerSpec.Operation(Map(
              "200" → SwaggerSpec.Response("")
            )))
          )
        )
      )
    }
  }
}

trait Spec
  extends FunSpec
  with ShouldMatchers
  with ScalatestRouteTest
  with SprayJsonSupport {

  def testRoute[T](testRoute: TestRoute)(body: ⇒ T) = {
    Get("/swagger.json") ~> testRoute.route ~> check(body)
  }
}

abstract class TestRoute(implicit val actorRefFactory: ActorRefFactory)
  extends HttpService {
  def route: Route
}
