-module(example_routes).

-export([build/0]).

build() ->
    nine:compile(example_router, todo_config()).

todo_config() ->
    #{<<"/">> => #{<<"GET">> => {todo_handler, get_todos}},
      <<"/todo">> => [{nine_mid, urlencoded_params},
                      #{<<"POST">> => {todo_handler, post_todo}}],
      <<"/todo/:id">> => #{<<"GET">> => {todo_handler, get_todo}},
      <<"/api">> => [#{<<"/todo/:id">> => #{<<"GET">> => {todo_handler, get_todo_json}},
                       <<"/todo">> => [{nine_mid, json_request},
                                     #{<<"POST">> => {todo_handler, post_todo_json},
                                       <<"PUT">> => {todo_handler, update_todo_json},
                                       <<"DELETE">> => {todo_handler, delete_todo_json}}],
                       <<"/todos">> => #{<<"GET">> => {todo_handler, get_todos_json}}},
                     {nine_mid, json_response}],
      <<"*">> => #{<<"_">> => {nine_util, not_found}}}.

config6() ->
    #{<<"/foo">> => #{<<"GET">> => {example_handler, get}}}.

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
       <<"/stuff2">> => #{<<"GET">> => [{example_mid, halt}, {example_handler, get}]},
       <<"/nest">> =>
           #{<<"/nested">> => #{<<"GET">> => {example_handler, get}},
             <<"/other">> => #{<<"GET">> => {example_handler, get}}},
       <<"/nest2">> =>
           [{example_mid, message}, #{<<"/nesty">> => #{<<"GET">> => {example_handler, get3}}}],
       <<"/">> => #{<<"GET">> => {example_handler, get}}}].

% <<"*">> => #{<<"_">> => {example_handler, get}}
