# wip-swagger-test

Tag a route with the `@swagger` annotation:

```scala
@swagger
val route =
  get {
    path("hello" / Segment) { name =>
      complete {
        s"hello, $name"
      }
    }
  }
```

The macro annotation prepends a `get & path("swagger.json")` route to the
annotated route that returns the Swagger spec in JSON.

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
