-module(example_mid).

-export([response/1]).

response(#{resp :=
               #{status := Status,
                 headers := Headers,
                 body := Body}}) ->
    {Status, Headers, Body}.
