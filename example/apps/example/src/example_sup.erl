%%%-------------------------------------------------------------------
%% @doc example top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(example_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    nine:compile(example_router, routes()),
    SupFlags =
        #{strategy => one_for_all,
          intensity => 0,
          period => 1},
    ElliOpts = [{callback, example_router}, {port, 3000}],
    ChildSpecs =
        [{example_http, {elli, start_link, [ElliOpts]}, permanent, 5000, worker, [elli]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
routes() ->
    [{example_mid, message},
     #{<<"/todos">> => #{<<"GET">> => {example_handler, get}},
       <<"/foo">> => #{<<"GET">> => [{example_handler, get2}, {example_mid, response}]},
       <<"/bar">> => [#{<<"GET">> => {example_handler, get2}}, {example_mid, response}],
       <<"/stuff">> => #{<<"GET">> => {example_handler, get3}},
       <<"/">> => #{<<"GET">> => {example_handler, get}}}].
