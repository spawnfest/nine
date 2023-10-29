-module(example_handler).

-export([get/1, get2/1, get3/1, index/1, get_param/1]).

get(_Context) ->
    {ok, [], <<"Hello World!">>}.

get2(Context) ->
    Context#{resp =>
                 #{status => ok,
                   headers => [],
                   body => <<"Hello World!">>}}.

get3(#{message := Message}) ->
    {ok, [], Message}.

index(_) ->
    {ok, [], <<"Hello Index">>}.

get_param(#{params := #{id := Id}}) ->
    {ok, [], list_to_binary([<<"Todo ">>, Id])}.
