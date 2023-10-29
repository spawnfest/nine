-module(nine_mid).

-export([urlencoded_params/1]).

urlencoded_params(Context=#{req := Req}) ->
    case elli_request:get_header(<<"Content-Type">>, Req) of
        <<"application/x-www-form-urlencoded">> ->
            PostParams = maps:from_list(elli_request:post_args_decoded(Req)),
            case Context of
                #{params := Params} ->
                    Context#{params => maps:merge(Params, PostParams)};
                _ ->
                    Context#{params => PostParams}
            end;
        _ ->
            Context#{here => hello}
    end.
