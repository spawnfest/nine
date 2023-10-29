-module(example_routes).

-export([build/0]).

build() ->
    nine:compile(example_router, config5()).

config5() ->
    #{<<"/todo/:id">> => #{<<"GET">> => {example_handler, get_param}},
      <<"/assets">> => #{<<"*">> => #{<<"GET">> => {example_handler, index}}},
      <<"*">> => #{<<"GET">> => {example_handler, get}}}.

config4() ->
    #{<<"/todo/:id">> => #{<<"GET">> => {example_handler, get_param}},
      <<"/todo/foo">> => #{<<"GET">> => {example_handler, get}},
      <<"/foo">> => #{<<"GET">> => {example_handler, get}}}.

config3() ->
    #{<<"/">> => #{<<"GET">> => {example_handler, index}}}.

config2() ->
    #{<<"/nest">> =>
          #{<<"/nested">> =>
                [{example_mid, message},
                 #{<<"/super">> => #{<<"GET">> => {example_handler, get3}}}]}}.

config() ->
    [{example_mid, message},
     #{<<"/todos">> => #{<<"GET">> => {example_handler, get}},
       <<"/foo">> => #{<<"GET">> => [{example_handler, get2}, {example_mid, response}]},
       <<"/bar">> => [#{<<"GET">> => {example_handler, get2}}, {example_mid, response}],
       <<"/stuff">> => #{<<"GET">> => {example_handler, get3}},
       <<"/splat">> => #{<<"_">> => {example_handler, get}},
       <<"/nest">> =>
           #{<<"/nested">> => #{<<"GET">> => {example_handler, get}},
             <<"/other">> => #{<<"GET">> => {example_handler, get}}},
       <<"/nest2">> =>
           [{example_mid, message}, #{<<"/nesty">> => #{<<"GET">> => {example_handler, get3}}}],
       <<"/">> => #{<<"GET">> => {example_handler, get}}}].

% <<"*">> => #{<<"_">> => {example_handler, get}}
