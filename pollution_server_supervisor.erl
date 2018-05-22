%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. maj 2018 16:23
%%%-------------------------------------------------------------------
-module(pollution_server_supervisor).
-behaviour(supervisor).
-author("Prezes").

%% API
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, supervisor}, ?MODULE, []).

init([]) ->
  {ok, {
    {one_for_one, 10, 1000},
    [{pollution_gen_server,
      {pollution_gen_server, start_link, []},
      permanent, 5000, worker, [pollution_gen_server]}
    ]
  }
  }.






















