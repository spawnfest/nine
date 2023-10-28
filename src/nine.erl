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
    codegen_handle() ++ codegen_handle_event() ++ codegen_routes(config_to_routes(Config)).

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

config_to_routes(Config) ->
    lists:flatten(
        lists:map(fun extract_path_method_handlers/1, maps:to_list(Config))).

extract_path_method_handlers({Path0, PathConfig}) ->
    Path = codegen_path(parse_path(Path0)),
    extract_method_handlers(Path, PathConfig).

extract_method_handlers(Path, PathConfig = [_ | _]) ->
    {MidBefore, Config, MidAfter} = split_middleware_config(PathConfig),
    lists:map(fun({M, MethodConfig}) ->
                 MethodConfig2 =
                     case MethodConfig of
                         [_ | _] = Mc ->
                             MidBefore ++ Mc ++ MidAfter;
                         {_Module, _Function} = Mc ->
                             MidBefore ++ [Mc] ++ MidAfter
                     end,
                 Behavior = method_config_to_behavior(MethodConfig2),
                 {Path, translate_method(M), Behavior}
              end,
              maps:to_list(Config));
extract_method_handlers(Path, PathConfig = #{}) ->
    lists:map(fun({M, MethodConfig}) ->
                 Behavior = method_config_to_behavior(MethodConfig),
                 {Path, translate_method(M), Behavior}
              end,
              maps:to_list(PathConfig)).

split_middleware_config(Config) ->
    split_middleware_config(Config, [], undefined, undefined).

split_middleware_config([], Before, Config2, After) ->
    {lists:reverse(Before), Config2, lists:reverse(After)};
split_middleware_config([Item | Rest], Before, undefined, undefined) ->
    case Item of
        #{} = Item2 ->
            split_middleware_config(Rest, Before, Item2, []);
        {_Module, _Function} = Item2 ->
            split_middleware_config(Rest, [Item2 | Before], undefined, undefined)
    end;
split_middleware_config([Item | Rest], Before, Config, After) ->
    split_middleware_config(Rest, Before, Config, [Item | After]).

method_config_to_behavior({Module, Function}) ->
    [{call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Function}}, [{var, 0, 'Req'}]}];
method_config_to_behavior([_ | _] = Middleware) ->
    middleware_to_behavior(Middleware).

middleware_to_behavior(Middleware) ->
    middleware_to_behavior(Middleware, [], 0).

middleware_to_behavior([], Acc, Counter) ->
    lists:reverse([{var, 0, req_atom(Counter)} | Acc]);
middleware_to_behavior([{Module, Function} | Middleware], Acc, Counter) ->
    Acc2 = [handler_to_behavior(Module, Function, Counter) | Acc],
    middleware_to_behavior(Middleware, Acc2, Counter + 1).

req_atom(Counter) ->
    binary_to_atom(list_to_binary(["Req", integer_to_list(Counter)])).

handler_to_behavior(Module, Function, 0) ->
    Req1 = 'Req',
    Req2 = req_atom(1),
    codegen_match_handle_req(Req1, Req2, Module, Function);
handler_to_behavior(Module, Function, Counter) ->
    Req1 = req_atom(Counter),
    Req2 = req_atom(Counter + 1),
    codegen_match_handle_req(Req1, Req2, Module, Function).

codegen_match_handle_req(Req1, Req2, Module, Function) ->
    {match, 0, {var, 0, Req2}, codegen_handle_req(Module, Function, Req1)}.

codegen_handle_req(Module, Function, Req) ->
    {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Function}}, [{var, 0, Req}]}.

codegen_routes(Routes) ->
    [{function, 0, handle, 1, lists:map(fun codegen_route/1, Routes)}].

codegen_route({Path, Method, Behavior}) ->
    codegen_method_clause(Path, Method, Behavior).

parse_path(Path0) ->
    [_ | Rest] = string:split(Path0, "/", all),
    Rest.

codegen_path([<<>>]) ->
    {nil, 0};
codegen_path([[]]) ->
    {nil, 0};
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
