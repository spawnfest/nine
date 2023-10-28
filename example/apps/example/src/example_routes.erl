-module(example_routes).

-export([build/0]).

build() ->
    nine:compile(example_router, config2()).

config2() ->
    #{<<"/nest">> => #{<<"/nested">> => #{<<"GET">> => {example_handler, get}}}}.

config() ->
    [{example_mid, message},
     #{<<"/todos">> => #{<<"GET">> => {example_handler, get}},
       <<"/foo">> => #{<<"GET">> => [{example_handler, get2}, {example_mid, response}]},
       <<"/bar">> => [#{<<"GET">> => {example_handler, get2}}, {example_mid, response}],
       <<"/stuff">> => #{<<"GET">> => {example_handler, get3}},
       <<"/splat">> => #{<<"_">> => {example_handler, get}},
       %       <<"/nest">> => #{<<"/nested">> => #{<<"GET">> => {example_handler, get_nested1}},
       %                        <<"/other">> => #{<<"GET">> => {example_handler, get_nest}}},
       %       <<"/nest2">> => [{example_mid, message},
       %                        #{<<"/nesty">> => #{<<"GET">> => {example_handler, get_nest3}}}],
       <<"/">> => #{<<"GET">> => {example_handler, get}},
       <<"*">> => #{<<"_">> => {example_handler, get}}}].
