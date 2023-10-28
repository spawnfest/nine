-module(reference_handler).
-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

handle(Req, _Args) ->
    handle_req(Req#req.method, elli_request:path(Req), Req).

handle_req('GET', [], Req) ->
    example_handler:get(Req);
handle_req('GET', [<<"todos">>], Req) ->
    example_handler:get(Req);
handle_req('GET', [<<"foo">>], Req) ->
    example_handler:get(Req).

handle_event(_Event, _Data, _Args) ->
    ok.
