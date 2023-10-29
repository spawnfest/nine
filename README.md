# nine

A data-driven routing library for [elli](https://github.com/elli-lib/elli) web servers.

`nine` consists of a router compiler, middleware, and utility functions. You can think of it as a micro web framework.

The goal of `nine` is to allow developers to precisely compose middleware in an intuitive way.
A side effect of this is that it makes writing web servers with elli more accessible. Despite elli's
performance, it is not as widely used. `nine` allows developers to not have to choose between performance and ease of use.

## Build

    rebar3 compile

## Demo 

    cd example
    rebar3 compile
    rebar3 shell

Navigate to localhost:3000. Please see `example/README.md` for more details.

## How It Works

`nine:compile` takes a router config and compiles it into an Erlang module at runtime using [forms](https://www.erlang.org/doc/apps/erts/absform).

The handle function generated has a few important details. It will wrap the elli req and create Context map that will always have these keys:

    #{path => elli_request:path(Req),
      method => Req#req.method,
      req => Req}

If URL params are specified as part of the route then `params` key will also be included with the corresponding named params in  map.

So if the path was /foo/:name/:id, then the Context map will look like this:

    #{params => #{name => <<"hackercat">>, id => <<"42">>},
      path => ...,
      method => ...,
      req => ...}

Halting is implemented by wrapping each call to middleware and handlers in a case statement checking for a `response` key.
If such a key exists, then response is immediately sent, instead of triggering further middleware.

A middleware chain like this:

    [{nine_mid, json_request}, {todo_handler, post_todo}]

Will generate Erlang forms along the lines of:

    case nine_mid:json_request(Req) of
         #{response := _} = Resp ->
             Resp;
         Req1 ->
             case todo_handler:post_todo(Req1) of
                 #{response := _} = Resp1 ->
                     Resp1;
                 Req2 ->
                     Req2
             end
    end         

## Basic Usage

Write a request handler:

    -module(basic).
    -export([index/1]).

    index(_Context) ->
        {200, [], <<"Hello World!">>}.

The index function returns a standard elli response.        

Compile a router config:

    nine:compile(example_router, #{<<"/">> => #{<<"GET">> => {basic, index}}}).

This will generate a module `example_router` that provides two functions `handle/2`, and `handle_event/3`.

Then you can initialize your elli web server in your apps supervisor like so:

    init([]) ->
        SupFlags =
            #{strategy => one_for_all,
              intensity => 0,
              period => 1},
        ElliOpts = [{callback, example_router}, {port, 3000}],
        ChildSpecs =
            [{example_http, {elli, start_link, [ElliOpts]}, permanent, 5000, worker, [elli]}],
        {ok, {SupFlags, ChildSpecs}}.

Notice how `example_router` is specified as the callback.        

## Router Config

### Handler

A handler is specified as {module, function}. Example:

    {basic_handlers, get_todo}

`basic_handlers` being the module, and `get_todo` is the function. `nine` compiler will take this handler
config and generate a function call.

### Method Configs

A method config is a map with keys corresponding to HTTP methods and a value that is a handler.

    #{<<"GET">> => {todo_handler, get_todo},
      <<"POST">> => {todo_handler, post_todo},
      <<"PUT">> => {todo_handler, put_todo},
      <<"PATCH">> => {todo_handler, patch_todo},
      <<"DELETE">> => {todo_handler, delete_todo},
      <<"_">> => {todo_handler, any_todo}}

The example above demonstrates the set of keys possible in a method config. _ will match any method.

### Path Configs

A path config is a map with url paths or segments as keys and method configs or path configs as values.

    #{<<"/">> => #{<<"GET">> => {basic_handler, index}},
      <<"/foo">> => #{<<"POST">> => {basic_handler, post_foo}}}.

This Path Config will route to the paths / and /foo.

#### Nested Path Configs

A value in a path config can also be another path config. This example demonstrates nested path configs:

    #{<<"/api">> => #{<<"/v1">> => #{<<"GET">> => {basic_handler, index}},
                      <<"/v2">> => #{<<"GET">> => {basic_handler2, index}}}}.

This demonstrates how one can describe multiple api versions with a nested path config.
The paths for these are "/api/v1" and "/api/v2".

Due to implementation details the above config is equivalent to this:

    #{<<"/api/v1">> => #{<<"GET">> => {basic_handler, index}},
      <<"/api/v2">> => #{<<"GET">> => {basic_handler2, index}}}.

Despite the equivalence, we will see how nesting is key to precisely composing middleware.

#### URL Path Params

`nine` builds in a way to have named parameters in the URL.

A path like `/todo/:id` will result in the context map including the params key.
The value of the params will be `#{id => <<"id1">>}`.
In case you are worried about atoms coming from user data, it is okay for `id` to be an atom because it is a static value set at compile time. 

#### Wildcard

`nine` provides a special path, `<<"*">>` to match anything. To match any method use `<<"_">>`. For example:

    #{<<"*">> => #{<<"_">> => {nine_util, not_found}}}

will match any request with any method and return a 404 not found response.

Using `<<"*">>` in nested path configs will result in the behavior of an ignored named param rather than matching all.

For example:

    #{<<"/foo">> => #{<<"*">> => {foo_handler, whatever}}}

Will match a path prefixed with /foo and one more segment, such as "/foo/bar", not "/foo/bar/baz".

#### Path Sorting

A number of `nine`'s features conflict with the linear nature of Erlang's pattern matching. How do wildcards and path params not shadow other requests? More importantly, how is order guaranteed when the config is a map?

After flattening the router config, `nine` sorts the routes by the aggregated path value. The sorting function works alphabetically in addition to these rules:

    <<"/foo/bar">> < <<"/foo/:id">> < <<"/foo/*">> < <<"*">>

If a path is less than another it will precede it in the list. The sorting function is designed to minimize shadowing.   

The sorting function is a bit complicated so its possible edge cases are there, but for the most part, this works well. It even allows router configs like this:

    #{<<"/todo/:id">> => #{<<"GET">> => {basic_handler, get_todo}},
      <<"/todo/foo">> => #{<<"GET">> => {basic_handler, get_foo}}}.

This means requests at /todo/foo will always hit the handler `basic_handler:get_foo`, despite the /todo/:id being a path param.      

### Middleware

Middleware are specified just like handlers, in fact they are the same thing! An example middleware might look like:

    {nine_mid, json_response}

Middleware are functions that take a Context as input and output a Context or an elli response. One could write a logging middleware like this:

    logging_middleware(Context) ->
        logger:debug(#{context => Context}),
        Context.

Or we could make a middleware that adds some data to the Context:

    message_middleware(Context) ->
        Context#{message => <<"Hello, World!">>}.

Middleware are helpful in all sorts of situations and allow developers to write web apps in a DRY way.

#### Middleware Chains

`nine` specifies middleware chaining with lists in the router config. Middleware chains can wrap path configs, method configs, and handlers.
The order specified in the chain determines the order of execution.

For example:

    [{nine_mid, urlencoded_params}, {todo_handler, post_todo}]

Will generate a sequence of function calls where `nine_mid:urlencoded_params` is called first, and the result is passed to
`todo_handler:post_todo`.

Allowing `post_todo` to be implemented like so:

    post_todo(Context=#{params := #{<<"body">> := Body}}) ->
        todo_db:insert(Body),
        nine_util:redirect(Context, <<"/">>).

`post_todo` can expect the params key to be filled with data because `urlencoded_params` is called first.

Middleware chains can wrap method configs and path configs. Middleware chains can even wrap the entire router config!

    #{<<"/api">> => [{nine_mid, json_request},
                     #{<<"POST">> => {todo_handler, post_todo_json},
                       <<"DELETE">> =>{todo_handler, delete_todo_json}}]}.

The middleware `nine_mid:json_request` is specified to occur before all the handlers in the method config.

Here is an example of a middleware chain around the entire config:

    [{example_mid, log_request},
     #{<<"/">> => #{<<"GET">> => {example_handler, stuff}}},
     {example_mid, log_response}].

Here any request will trigger `log_request` at the beginning of the pipeline, and `log_response` at the end of the pipeline.

We can combine middleware chaining with nested path configs:

    #{<<"/api">> => [{example_mid, v1},
                     #{<<"/v1">> => #{<<"GET">> => {example_handler, get_thing}},
                       <<"/other">> => #{<<"GET">> => {example_handler, other}}}],
      <<"*">> => #{<<"_">> => {nine_mid, not_found}}}.

#### Halting

There are situations where we want to return a response immediately without finishing the middleware chain. This is known as halting.

`nine` makes this possible because each middleware and handler call is wrapped in a case statement checking for the `response` key.

If a handler or middleware returns a Context map with the `response` key it will immediately be sent without triggering further middleware.

## Inspirations

`nine` was inspired by other composable middleware tools.

- [ring](https://github.com/ring-clojure/ring/wiki/Concepts) - Standard Clojure HTTP abstraction for web servers
- [ataraxy](https://github.com/weavejester/ataraxy) - data driven routing library for Clojure
- [Plug.Router](https://hexdocs.pm/plug/readme.html#plug-router) - Ecosystem defining Elixir HTTP middleware
- [golang http middleware](https://dev.to/theghostmac/understanding-and-crafting-http-middlewares-in-go-3183) - Standard Library Golang Middleware Pattern

## Fun Facts

- The name `nine` comes from "nine nines".
- Middleware was originally intended to look like Ring's, but wasn't compatible with Erlang's pattern matching lookups.
