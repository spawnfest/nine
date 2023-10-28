-module(example_handler).

-export([get/1]).

get(Context) ->
    {ok, [], <<"Hello World!">>}.
