-module(example_mid).

-export([response/1, message/1]).

response(#{resp :=
               #{status := Status,
                 headers := Headers,
                 body := Body}}) ->
    {Status, Headers, Body}.

message(Context) ->
    Context#{message => <<"This is a message!">>}.
