# nine

A router module compiler that allows developers to configure routes, middleware, and handlers with a 
declarative data config.

Could be considered a micro web framework or a foundational component of one.

Takes a map configuration and compiles into a elli callback module.

This library is meant to make using elli http server more ergonomic.

## Build

    rebar3 compile

## Usage

    cd example
    rebar3 compile
    rebar3 shell

Navigate to localhost:3000.

## Todos

- Basic default middleware
- Config validator
