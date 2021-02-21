The hand-written OpenAPI specification is `task.yaml`.

The server code is generated with:

    java -jar swagger-codegen-cli.jar generate -i task.yaml -l go-server -o ./swag-server

Where `swagger-codegen-cli.jar` is the downloaded binary (for OpenAPI v3) from
the Swagger Codegen website.

Note that this also creates a copy of the spec in
`./swag-server/api/swagger.yaml`. That copy is slightly different from the
input `task.yaml` - it's canonicalized and reformatted by the code generator.

Had to rename `go` dir to `swagger` to align with package name (for modules).

`task-swagger-2.json` is our API converted to Swagger (OpenAPI v2) using
the online tool https://lucybot-inc.github.io/api-spec-converter/

oapi-server builds upon code generated with the deepmap/oapi-codegen tool:

    oapi-codegen -package task -generate types,server task.yaml > oapi-server/internal/task/task.gen.go

The .gen.go file is unmodified except the addition of the "delete all" handler,
which isn't part of the public API.
