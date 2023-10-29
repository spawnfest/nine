-module(nine_mid).

-export([urlencoded_params/1, json_request/1, json_response/1]).

urlencoded_params(Context = #{req := Req}) ->
    case elli_request:get_header(<<"Content-Type">>, Req) of
        <<"application/x-www-form-urlencoded">> ->
            PostParams =
                maps:from_list(
                    elli_request:post_args_decoded(Req)),
            case Context of
                #{params := Params} ->
                    Context#{params => maps:merge(Params, PostParams)};
                _ ->
                    Context#{params => PostParams}
            end;
        _ ->
            Context
    end.

json_request(Context = #{req := Req}) ->
    case elli_request:get_header(<<"Content-Type">>, Req) of
        <<"application/json">> ->
            case thoas:decode(
                     elli_request:body(Req))
            of
                {ok, Json} ->
                    Context#{json => Json};
                _ ->
                    Context#{response => {500, [], <<"Encoding Error">>}}
            end;
        _ ->
            Context#{response => {400, [], <<"Expected JSON">>}}
    end.

json_response(#{draft := Draft, status := Status}) ->
    {Status, [{<<"Content-Type">>, <<"application/json">>}], thoas:encode(Draft)};
json_response(#{draft := Draft}) ->
    {200, [{<<"Content-Type">>, <<"application/json">>}], thoas:encode(Draft)};
json_response(Context) ->
    Context.
