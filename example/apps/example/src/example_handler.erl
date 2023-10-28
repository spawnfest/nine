-module(example_handler).

-export([get/1, get2/1]).

get(Context) ->
    {ok, [], <<"Hello World!">>}.

get2(Context) ->
    Context#{resp =>
                 #{status => ok,
                   headers => [],
                   body => <<"Hello World!">>}}.
