set dotenv-load

# just displays the receipes
default:
    @just --list

# builds the project
build:
    stack build

# runs the project
run:
    stack run

# runs the HTTP file
http file:
    docker run --rm -i -t -v $PWD/http:/workdir jetbrains/intellij-http-client \
    --env-file http-client.env.json --env dev -D {{file}}.http
