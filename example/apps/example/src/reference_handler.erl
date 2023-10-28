-module(reference_handler).
-export([handle/2, handle_event/3]).

handle(Req, _Args) ->
    example_handler:get(Req).

handle_event(_Event, _Data, _Args) ->
    ok.
