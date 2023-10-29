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
    codegen_handle()
    ++ codegen_handle_event()
    ++ codegen_routes(sort_routes(config_to_routes(Config))).

%% true means A < B
sort_routes(Routes) ->
    lists:sort(fun({A, AMethod, _}, {B, BMethod, _}) ->
                  case A == B of
                      true ->
                          AMethod < BMethod;
                      false ->
                          AContainsParams = contains_params(A),
                          BContainsParams = contains_params(B),
                          ASplat = contains_splat(A),
                          BSplat = contains_splat(B),
                          case {AContainsParams, BContainsParams, ASplat, BSplat} of
                              {true, true, true, true} ->
                                  AIsSplat = A == <<"*">>,
                                  BIsSplat = B == <<"*">>,
                                  case {AIsSplat, BIsSplat} of
                                      {true, false} ->
                                          false;
                                      {false, true} ->
                                          true;
                                      _ ->
                                          A < B
                                  end;
                              {true, false, true, true} ->
                                  AIsSplat = A == <<"*">>,
                                  BIsSplat = B == <<"*">>,
                                  case {AIsSplat, BIsSplat} of
                                      {true, false} ->
                                          false;
                                      {false, true} ->
                                          true;
                                      _ ->
                                          false
                                  end;
                              {false, true, true, true} ->
                                  AIsSplat = A == <<"*">>,
                                  BIsSplat = B == <<"*">>,
                                  case {AIsSplat, BIsSplat} of
                                      {true, false} ->
                                          false;
                                      {false, true} ->
                                          true;
                                      _ ->
                                          true
                                  end;
                              {_, _, true, false} ->
                                  false;
                              {_, _, false, true} ->
                                  true;
                              {true, true, _, _} ->
                                  A < B;
                              {true, false, _, _} ->
                                  false;
                              {false, true, _, _} ->
                                  true;
                              {false, false, _, _} ->
                                  A < B
                          end
                  end
               end,
               Routes).

contains_params(S) ->
    case string:find(S, ":") of
        nomatch ->
            false;
        _ ->
            true
    end.

contains_splat(S) ->
    case string:find(S, "*") of
        nomatch ->
            false;
        _ ->
            true
    end.

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
    lists:flatten(config_to_routes2(<<"">>, Config)).

config_to_routes2(ParentPath, [_ | _] = Config) ->
    {MidBefore, Config2, MidAfter} = split_middleware_config(Config),
    config_to_routes_helper(ParentPath, Config2, MidBefore, MidAfter);
config_to_routes2(ParentPath, #{} = Config) ->
    config_to_routes_helper(ParentPath, Config, [], []).

config_to_routes_helper(ParentPath, Config, MidBefore, MidAfter) ->
    lists:map(fun({K, V}) ->
                 NewPath = list_to_binary([ParentPath, translate_splat(K)]),
                 case is_path_method_config(V) of
                     true ->
                         case V of
                             #{} = V2 ->
                                 extract_method_handlers(NewPath, MidBefore ++ [V2] ++ MidAfter);
                             [_ | _] = V2 ->
                                 extract_method_handlers(NewPath, MidBefore ++ V2 ++ MidAfter)
                         end;
                     false ->
                         case V of
                             #{} = V2 ->
                                 config_to_routes_helper(NewPath, V2, MidBefore, MidAfter);
                             [_ | _] = V2 ->
                                 {MB2, C2, MA2} = split_middleware_config(V2),
                                 config_to_routes_helper(NewPath,
                                                         C2,
                                                         MidBefore ++ MB2,
                                                         MA2 ++ MidAfter)
                         end
                 end
              end,
              maps:to_list(Config)).

translate_splat(<<"*">>) ->
    <<"/*">>;
translate_splat(S) ->
    S.

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
                 {Path, M, MethodConfig2}
              end,
              maps:to_list(Config)).

is_path_method_config(Config = #{}) ->
    lists:foldl(fun(K, Acc) ->
                   case Acc of
                       true ->
                           true;
                       false ->
                           case K of
                               <<"GET">> ->
                                   true;
                               <<"POST">> ->
                                   true;
                               <<"PUT">> ->
                                   true;
                               <<"PATCH">> ->
                                   true;
                               <<"DELETE">> ->
                                   true;
                               <<"_">> ->
                                   true;
                               _ ->
                                   false
                           end
                   end
                end,
                false,
                maps:keys(Config));
is_path_method_config(Config = [_ | _]) ->
    {_, Config2, _} = split_middleware_config(Config),
    is_path_method_config(Config2).

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

codegen_routes(Routes) ->
    [{function, 0, handle, 1, lists:map(fun codegen_route/1, Routes)}].

codegen_route({Path0, Method, Middleware}) ->
    PathParsed = parse_path(Path0),
    Behavior = middleware_to_behavior(Middleware, filter_path_params(PathParsed)),
    Path = codegen_path(PathParsed),
    codegen_method_clause(Path, Method, Behavior).

filter_path_params(Path) ->
    lists:filter(fun is_atom/1, Path).

middleware_to_behavior([_ | _] = Middleware, []) ->
    M = codegen_case_chain_init(m2b(Middleware, [], 0)),
    [M];
middleware_to_behavior([_ | _] = Middleware, PathParams) ->
    Pp = codegen_path_params(PathParams),
    M = codegen_case_chain_init(m2b(Middleware, [], 1)),
    [Pp, M].

codegen_path_params(PathParams) ->
    {match,
     0,
     {var, 0, 'Req1'},
     {map,
      0,
      {var, 0, 'Req0'},
      [{map_field_assoc,
        0,
        {atom, 0, params},
        {map, 0, codegen_path_param_elements(PathParams)}}]}}.

codegen_path_param_elements(PathParams) ->
    lists:map(fun(P) ->
                 Lvar = binary_to_atom(string:lowercase(atom_to_binary(P))),
                 {map_field_assoc, 0, {atom, 0, Lvar}, {var, 0, P}}
              end,
              PathParams).

m2b([], Acc, _Counter) ->
    Acc;
m2b([{Module, Function} | Middleware], Acc, Counter) ->
    m2b(Middleware, [{Module, Function, Counter} | Acc], Counter + 1).

codegen_case_chain_init([{Module, Function, Counter} | Rest]) ->
    codegen_case_chain(Rest,
                       codegen_case_wrapper(Module,
                                            Function,
                                            Counter,
                                            {var, 0, req_atom(Counter + 1)})).

codegen_case_chain([], Acc) ->
    Acc;
codegen_case_chain([{Module, Function, Counter} | Mid], Acc) ->
    codegen_case_chain(Mid, codegen_case_wrapper(Module, Function, Counter, Acc)).

codegen_case_wrapper(Module, Function, Counter, Behavior) ->
    {'case',
     0,
     codegen_handle_req(Module, Function, req_atom(Counter)),
     [{clause,
       0,
       [{map, 0, [{map_field_exact, 0, {atom, 0, response}, {var, 0, resp_atom(Counter)}}]}],
       [],
       [{var, 0, resp_atom(Counter)}]},
      {clause, 0, [{var, 0, req_atom(Counter + 1)}], [], [Behavior]}]}.

req_atom(Counter) ->
    binary_to_atom(list_to_binary(["Req", integer_to_list(Counter)])).

resp_atom(Counter) ->
    binary_to_atom(list_to_binary(["Resp", integer_to_list(Counter)])).

codegen_handle_req(Module, Function, Req) ->
    {call, 0, {remote, 0, {atom, 0, Module}, {atom, 0, Function}}, [{var, 0, Req}]}.

parse_path(Path0) ->
    [_ | Rest] = string:split(Path0, "/", all),
    lists:map(fun translate_path_param/1, Rest).

translate_path_param(S) ->
    case string:prefix(S, ":") of
        nomatch ->
            S;
        Suffix ->
            binary_to_atom(string:titlecase(Suffix))
    end.

split_wildcards(<<"*">>) ->
    <<"*">>;
split_wildcards(S) ->
    Result = string:split(S, "*", all),
    [<<"*">> | Rest] =
        lists:foldl(fun(E, Acc) ->
                       case E of
                           <<>> ->
                               [<<"*">> | Acc];
                           B ->
                               [<<"*">>, B | Acc]
                       end
                    end,
                    [],
                    Result),
    lists:reverse(Rest).

codegen_path([]) ->
    {var, 0, '_'};
codegen_path([<<>>]) ->
    {nil, 0};
codegen_path([[]]) ->
    {nil, 0};
codegen_path(Path) ->
    codegen_path(lists:reverse(Path), {nil, 0}).

codegen_path([], Acc) ->
    Acc;
codegen_path([E | Rest], Acc) ->
    codegen_path(Rest, {cons, 0, codegen_path_element(E), Acc}).

codegen_path_element(E) ->
    case E of
        <<"*">> ->
            {var, 0, '_'};
        E when is_atom(E) ->
            {var, 0, E};
        E when is_binary(E) ->
            codegen_binary(E)
    end.

codegen_binary(B) ->
    {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(B)}, default, default}]}.

%% TODO: change name method to something else
codegen_method_clause(Path, Method, Behavior) ->
    {clause,
     0,
     [{match, 0, {var, 0, 'Req0'}, {map, 0, codegen_method_clause_helper(Path, Method)}}],
     [],
     Behavior}.

codegen_method_clause_helper(Path, Method) ->
    [{map_field_exact, 0, {atom, 0, method}, codegen_method_value(translate_method(Method))},
     {map_field_exact, 0, {atom, 0, path}, Path}].

codegen_method_value(Method) ->
    case Method of
        '_Method' = M ->
            {var, 0, M};
        M ->
            {atom, 0, M}
    end.

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
    'DELETE';
translate_method(<<"_">>) ->
    '_Method'.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

split_wildcards_test() ->
    ?assertEqual([<<"foo">>, <<"*">>, <<"bar">>], split_wildcards(<<"foo*bar">>)),

    ?assertEqual([<<"*">>, <<"foo">>], split_wildcards(<<"*foo">>)),

    ?assertEqual([<<"foo">>, <<"*">>], split_wildcards(<<"foo*">>)).

parse_path_test() ->
    ?assertEqual([<<"foo">>, <<"bar">>], parse_path(<<"/foo/bar">>)),

    ?assertEqual([<<"foo">>, 'Bar'], parse_path(<<"/foo/:bar">>)),
    ?assertEqual([<<>>], parse_path(<<"/">>)),

    ?assertEqual([], parse_path(<<"*">>)).

translate_path_param_test() ->
    ?assertEqual(<<"foo">>, translate_path_param(<<"foo">>)),
    ?assertEqual('Foo', translate_path_param(<<":foo">>)).

-endif.
