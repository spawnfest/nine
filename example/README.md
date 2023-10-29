# example

An example todo application using nine. Project was generated with `rebar3 new release example`.

Relevant source code can be found in `apps/example/src/`. The files that showcase nine are:

- `apps/example/src/example_routes.erl` - router configuration
- `apps/example/src/todo_handler.erl` - request handlers
- `apps/example/src/example_callback.erl` - elli callback module using generated `example_router:handle`

The router compilation and db initialization are triggered at startup in `apps/example/src/example_app.erl`.

It isn't necessary to make your own elli callback module since nine generates one. However, it is done in this way to log errors in `handle_event`.

## Build

    rebar3 compile

## Usage

Start the application:

    rebar3 shell

The site will be available at http://localhost:3000.

The basic web page allows users to view and create todo posts.

A json API is also available with the prefix `/api`. 

This API can be tested with `./test_api.sh`. The script will run a sequence of `curl` commands against the api.
