-module(todo_handler).

-export([get_todos/1, get_todo/1, post_todo/1, update_todo/1, delete_todo/1]).

get_todos(_Context) ->
    Todos = todo_db:all(),
    {ok, View} = example_index_view_dtl:render(#{todos => Todos}),
    {ok, [], View}.

get_todo(Context=#{params := #{id := Id}}) ->
    case todo_db:get(binary_to_integer(Id)) of
        {ok, Todo} ->
            {ok, View} = example_todo_view_dtl:render(Todo),
            {ok, [], View};
        {error, not_found} ->
            nine_util:not_found(Context)
    end.

post_todo(Context=#{params := #{body := Body}}) ->
    todo_db:insert(Body),
    nine_util:redirect(Context, <<"/">>).

update_todo(Context=#{params := #{id := Id, body := Body}}) ->
    todo_db:update(binary_to_integer(Id), Body),
    nine_util:redirect(Context, <<"/">>).

delete_todo(Context=#{params := #{id := Id}}) ->
    todo_db:delete(binary_to_integer(Id)),
    nine_util:redirect(Context, <<"/">>).
