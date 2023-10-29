# nine

A data-driven routing library for [elli](https://github.com/elli-lib/elli) web servers.

`nine` consists of a router compiler, middleware, and utility functions. You can think of it as a micro web framework.

The goal of `nine` is to allow developers to precisely compose middleware in an intuitive way.
A side effect of this is that it makes writing web servers with elli more accessible. Despite elli's
performance, it is not as widely used. `nine` allows developers to have
blazing speed and a pleasant developer experience.

## Build

    rebar3 compile

## Demo 

    cd example
    rebar3 compile
    rebar3 shell

Navigate to localhost:3000. Please see `example/README.md` for more details.

## Inspirations

`nine` was inspired by other composable middleware tools.

- [ring](https://github.com/ring-clojure/ring/wiki/Concepts) - Standard Clojure HTTP abstraction for web servers
- [ataraxy](https://github.com/weavejester/ataraxy) - data driven routing library for Clojure
- [Plug.Router](https://hexdocs.pm/plug/readme.html#plug-router) - Ecosystem defining Elixir HTTP middleware
- [golang http middleware](https://dev.to/theghostmac/understanding-and-crafting-http-middlewares-in-go-3183)

## Fun Facts

- The name `nine` comes from "nine nines".
- Middleware was originally intended to look like Ring's, but this was not done because of performance reasons.
