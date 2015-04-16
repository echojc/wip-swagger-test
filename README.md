# wip-swagger-test

Tag a route with the `@swagger` annotation:

```scala
@swagger
val route =
  get {
    path(...) {
      complete {
        "hi"
      }
    }
  }
```

The macro annotation prepends a `get & path("swagger.json")` route to the
annotated route.

Start the route with spray-can:

```scala
startServer(interface = "localhost", port = 8080) {
  route
}
```

Get the Swagger spec by navigating to http://localhost:8080/swagger.json. You
can explore it using, for example, the [Swagger
Editor](http://editor.swagger.io/).

See [Main.scala](core/src/main/scala/Main.scala) for a longer example.
