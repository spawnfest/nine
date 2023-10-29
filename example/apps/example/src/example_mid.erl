-module(example_mid).

-export([response/1, message/1, halt/1]).

response(#{resp :=
               #{status := Status,
                 headers := Headers,
                 body := Body}}) ->
    {Status, Headers, Body}.

message(Context) ->
    Context#{message => <<"This is a message!">>}.

halt(Context) ->
    Context#{response => {200, [], <<"Intercepted!">>}}.
