-module(example_routes).

-export([build/0]).

build() ->
    nine:compile(example_router, config()).

config() ->
    [{example_mid, message},
     #{<<"/todos">> => #{<<"GET">> => {example_handler, get}},
       <<"/foo">> => #{<<"GET">> => [{example_handler, get2}, {example_mid, response}]},
       <<"/bar">> => [#{<<"GET">> => {example_handler, get2}}, {example_mid, response}],
       <<"/stuff">> => #{<<"GET">> => {example_handler, get3}},
       <<"/splat">> => #{<<"*">> => {example_handler, get}},
       <<"/">> => #{<<"GET">> => {example_handler, get}},
       <<"*">> => #{<<"*">> => {example_handler, get}}}].
