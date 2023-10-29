-module(example_callback).

-export([handle/2, handle_event/3]).

-include_lib("elli/include/elli.hrl").

-behavior(elli_handler).

handle(Req, Args) ->
    example_router:handle(Req, Args).

handle_event(request_error, Data, _Args) ->
    logger:error(#{error => Data, event => request_error});
handle_event(invalid_return, Data, _Args) ->
    logger:error(#{error => Data, event => invalid_return});
handle_event(Event, Data, _Args) ->
    logger:debug(#{event => Event, data => Data}).
