%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 18. maj 2018 14:48
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Prezes").
-behaviour(gen_server).
-export([init/1, handle_call/3, terminate/2]).
%% API
-export([start_link/0, stop/0]).
-export([addStation/2, addValue/4, removeValue/3]).
-export([ getOneValue/3, getStationMean/2, getDailyMean/2, getAreaMean/3]).

%% User Interface

start_link() ->
  gen_server:start_link({local, pollution_gen_server}, ?MODULE, [], []).
stop() -> gen_server:call(pollution_gen_server, {stop}).
addStation(Name, {X, Y}) ->
  gen_server:call(pollution_gen_server, {addStation, Name, {X, Y}}).
addValue(StationInfo, Date, Type, Value) ->
  gen_server:call(pollution_gen_server, {addValue, StationInfo, Date, Type, Value}).
removeValue(StationInfo, Date, Type) ->
  gen_server:call(pollution_gen_server, {removeValue, StationInfo, Date, Type}).
getOneValue(Type, Date, StationInfo) ->
  gen_server:call(pollution_gen_server, {getOneValue, Type, Date, StationInfo}).
getStationMean(Type, StationInfo) ->
  gen_server:call(pollution_gen_server, {getStationMean, Type, StationInfo}).
getDailyMean(Type, Date) ->
  gen_server:call(pollution_gen_server, {getDailyMean, Type, Date}).
getAreaMean(StationInfo, Type, Radius) ->
  gen_server:call(pollution_gen_server, {getAreaMean, StationInfo, Type, Radius}).



%% callbacks

init([]) -> {ok, pollution:createMonitor()}.
handle_call({stop}, _, Monitor) -> {stop, normal, ok, Monitor};

handle_call({addStation, Name, {X, Y}}, _, Monitor) ->
  R = pollution:addStation(Name, {X, Y}, Monitor),
  verify_status_response(R, Monitor);
handle_call({addValue, StationInfo, Date, Type, Value}, _, Monitor) ->
  R = pollution:addValue(StationInfo, Date, Type, Value, Monitor),
  verify_status_response(R, Monitor);
handle_call({removeValue, StationInfo, Date, Type}, _, Monitor) ->
  R = pollution:removeValue(StationInfo, Date, Type, Monitor),
  verify_status_response(R, Monitor);
handle_call({getOneValue, Type, Date, StationInfo}, _, Monitor) ->
  R = pollution:getOneValue(Type, Date, StationInfo, Monitor),
  verify_get_response(R, Monitor);
handle_call({getStationMean, Type, StationInfo}, _, Monitor) ->
  R = pollution:getStationMean(Type, StationInfo, Monitor),
  verify_get_response(R, Monitor);
handle_call({getDailyMean, Type, Date}, _, Monitor) ->
  R = pollution:getDailyMean(Type, Date, Monitor),
  verify_get_response(R, Monitor);
handle_call({getAreaMean, StationInfo, Type, Radius}, _, Monitor) ->
  R = pollution:getAreaMean(StationInfo, Type, Radius, Monitor),
  verify_get_response(R, Monitor).


verify_get_response({error, Msg}, Monitor) ->
  {reply, Msg, Monitor};
verify_get_response(Value, Monitor) ->
  {reply, Value, Monitor}.

verify_status_response({error, Msg}, Monitor) ->
  {reply, Msg, Monitor};
verify_status_response(NewMonitor, _) ->
  {reply, ok, NewMonitor}.


terminate(_Reason, _State) ->
  ok.