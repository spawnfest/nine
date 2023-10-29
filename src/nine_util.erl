-module(nine_util).

-export([get_method/1, not_found/1, redirect/2]).

-include_lib("elli/include/elli.hrl").

get_method(Req) ->
    Req#req.method.

not_found(Context) ->
    Context#{response => {404, [], <<"Not Found">>}}.

redirect(Context, RedirectPath) ->
    Context#{response => {302, [{<<"Location">>, RedirectPath}], <<>>}}.
