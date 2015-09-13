# wip-swagger-test

Tag a trait with the `@swagged` annotation:

```scala
@swagged trait MyRoute {
  val route =
    get {
      path("hello" / Segment) { name =>
        complete {
          s"hello, $name"
        }
      }
    }
}
```

The macro annotation prepends a `get & path("swagger.json")` route to each route
defined as a `val` in that trait (a `val` is a route if it is typed as a
`spray.routing.Route`). The prepended route returns the Swagger spec for that
route.

To see it, start the route with spray-can:

```scala
startServer(interface = "localhost", port = 8080) {
  route
}
```

Navigate to http://localhost:8080/swagger.json:

```json
{
  "swagger": "2.0",
  "info": {
    "title": "wip",
    "version": "0"
  },
  "paths": {
    "/hello/{name}": {
      "get": {
        "parameters": [{
          "name": "name",
          "in": "path",
          "required": true,
          "type": "string"
        }],
        "responses": {
          "200": {
            "description": ""
          }
        }
      }
    }
  }
}
```

You can explore it using, for example, the [Swagger
Editor](http://editor.swagger.io/).

See [Main.scala](core/src/main/scala/Main.scala) for a longer example.
