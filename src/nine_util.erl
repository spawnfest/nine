-module(nine_util).

-export([get_method/1]).

-include_lib("elli/include/elli.hrl").

get_method(Req) ->
    Req#req.method.
