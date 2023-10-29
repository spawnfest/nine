-module(todo_db).

-export([init/0, all/0, get/1, insert/1, update/2, delete/1]).

init() ->
    ets:new(db, [ordered_set, public, named_table]),
    ets:insert(db, {count, 0}).

all() ->
    format_todos(filter_count(ets:match(db, '$1'))).

get(Id) ->
    case ets:lookup(db, Id) of
        [] ->
            {error, not_found};
        Todo ->
            {ok, format_todo(Todo)}
    end.

insert(Body) ->
    Count = get_count(),
    ets:insert(db, {Count, Body}),
    update_count(Count),
    Count.

get_count() ->
    [{count, Count}] = ets:lookup(db, count),
    Count.

update_count(Count) ->
    ets:insert(db, {count, Count + 1}).

update(Id, Body) ->
    ets:insert(db, {Id, Body}).

delete(Id) ->
    ets:delete(db, Id).

filter_count(Todos) ->
    lists:filter(fun([{Id, _}]) -> Id =/= count end, Todos).

format_todos(Todos) ->
    lists:map(fun format_todo/1, Todos).

format_todo([{Id, Body}]) ->
    #{id => Id, body => Body}.
