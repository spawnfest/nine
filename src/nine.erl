-module(nine).

-export([compile/2]).

compile(Module, RoutesMap) ->
    {ok, Forms} = routes_map_to_forms(Module, RoutesMap),
    {ok, Module, Bin} = compile:forms(Forms),
    load_module_binary(Module, Bin).

load_module_binary(Module, Bin) ->
    BeamFile = binary_to_list(list_to_binary([atom_to_list(Module), ".beam"])),
    {module, Module} = code:load_binary(Module, BeamFile, Bin).

-spec codegen_module_form(module()) -> erl_parse:erl_parse_tree().
codegen_module_form(Module) ->
    {attribute, 0, module, Module}.

routes_map_to_forms(Module, Config) ->
    {ok, [codegen_module_form(Module)] ++ elli_exports() ++ routes_map_to_methods(Config)}.

elli_exports() ->
    [{attribute, 0, export, [{handle, 2}, {handle_event, 3}]}].

routes_map_to_methods(Config) ->
    codegen_handle() ++ codegen_handle_event() ++ codegen_routes(Config).

codegen_handle_event() ->
    [{function,
      0,
      handle_event,
      3,
      [{clause,
        0,
        [{var, 0, '_Event'}, {var, 0, '_Data'}, {var, 0, '_Args'}],
        [],
        [{atom, 0, ok}]}]}].

codegen_handle() ->
    [{function,
      0,
      handle,
      2,
      [{clause,
        0,
        [{var, 0, 'Req'}, {var, 0, '_Args'}],
        [],
        [{call,
          0,
          {atom, 0, handle},
          [{map,
            0,
            [{map_field_assoc, 0, {atom, 0, req}, {var, 0, 'Req'}},
             {map_field_assoc, 0, {atom, 0, method}, codegen_get_method()},
             {map_field_assoc, 0, {atom, 0, path}, codegen_get_path()}]}]}]}]}].

codegen_get_method() ->
    {call, 1, {remote, 1, {atom, 1, nine_util}, {atom, 1, get_method}}, [{var, 1, 'Req'}]}.

codegen_get_path() ->
    {call, 0, {remote, 0, {atom, 0, elli_request}, {atom, 0, path}}, [{var, 0, 'Req'}]}.

codegen_routes(Config) ->
    [{function,
      0,
      handle,
      1,
      maps:fold(fun(K, V, Acc) -> [codegen_method_clause(K, V)] ++ Acc end, [], Config)}].

codegen_method_clause(K, V) ->
    Path0 = parse_path(K),
    Path = codegen_path(Path0),
    {Method, Handler} = extract_method_handler(V),
    Behavior = handler_to_behavior(Handler),
    codegen_method_clause(Path, Method, Behavior).

parse_path(Path0) ->
    [_ | Rest] = string:split(Path0, "/", all),
    Rest.

codegen_path(Path) ->
    codegen_path(lists:reverse(Path), {nil, 0}).

codegen_path([], Acc) ->
    Acc;
codegen_path([B | Rest], Acc) ->
    codegen_path(Rest,
                 {cons,
                  0,
                  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(B)}, default, default}]},
                  Acc}).

extract_method_handler(MethodConfig) ->
    [{Method, Handler}] = maps:to_list(MethodConfig),
    {translate_method(Method), Handler}.

handler_to_behavior({Module, Function}) ->
    [{call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Function}}, [{var, 0, 'Req'}]}].

codegen_method_clause(Path, Method, Behavior) ->
    {clause,
     0,
     [{match,
       0,
       {var, 0, 'Req'},
       {map,
        0,
        [{map_field_exact, 0, {atom, 0, method}, {atom, 0, Method}},
         {map_field_exact, 0, {atom, 0, path}, Path}]}}],
     [],
     Behavior}.

-spec translate_method(binary()) -> atom().
translate_method(<<"GET">>) ->
    'GET';
translate_method(<<"POST">>) ->
    'POST';
translate_method(<<"PUT">>) ->
    'PUT';
translate_method(<<"PATCH">>) ->
    'PATCH';
translate_method(<<"DELETE">>) ->
    'DELETE'.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-endif.
