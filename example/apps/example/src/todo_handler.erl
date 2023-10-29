-module(todo_handler).

-export([get_todos/1, get_todo/1, post_todo/1]).
-export([get_todos_json/1, get_todo_json/1, post_todo_json/1, update_todo_json/1,
         delete_todo_json/1]).

get_todos(_Context) ->
    Todos = todo_db:all(),
    {ok, View} = example_index_view_dtl:render(#{todos => Todos}),
    {ok, [], View}.

get_todo(Context = #{params := #{id := Id}}) ->
    case todo_db:get(binary_to_integer(Id)) of
        {ok, Todo} ->
            {ok, View} = example_todo_view_dtl:render(Todo),
            {ok, [], View};
        {error, not_found} ->
            nine_util:not_found(Context)
    end.

post_todo(Context = #{params := #{<<"body">> := Body}}) ->
    todo_db:insert(Body),
    nine_util:redirect(Context, <<"/">>).

get_todos_json(Context) ->
    Todos = todo_db:all(),
    Context#{draft => #{<<"todos">> => Todos}}.

get_todo_json(Context = #{params := #{id := Id}}) ->
    case todo_db:get(binary_to_integer(Id)) of
        {ok, Todo} ->
            Context#{draft => Todo};
        {error, not_found} ->
            Context#{draft => #{<<"error">> => <<"Not Found">>}, status => 404}
    end.

post_todo_json(Context = #{json := #{<<"body">> := Body}}) ->
    Id = todo_db:insert(Body),
    Context#{draft => #{<<"id">> => Id}}.

update_todo_json(Context = #{json := #{<<"id">> := Id, <<"body">> := Body}}) ->
    todo_db:update(Id, Body),
    Context#{draft => #{<<"status">> => <<"ok">>}}.

delete_todo_json(Context = #{json := #{<<"id">> := Id}}) ->
    todo_db:delete(Id),
    Context#{draft => #{<<"status">> => <<"ok">>}}.
