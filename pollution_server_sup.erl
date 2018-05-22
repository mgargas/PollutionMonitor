%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. maj 2018 14:16
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("Prezes").

%% API
-export([]).
-include("pollution_records.hrl").
-export([start_supervisor/0, supervisor/0]).

start_supervisor() -> spawn(?MODULE, supervisor, []).

supervisor() ->
  process_flag(trap_exit, true),
  register(pollutionServer, spawn_link(pollution_server, init, [])),
  receive
    {'EXIT', _, normal} -> io:format("Finished succesfully."), terminate();
    {'EXIT', _, _} -> io:format("Supervisor is about to resume this server."), supervisor();
    stop -> terminate()
  end.

terminate() -> ok.