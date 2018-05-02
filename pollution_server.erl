%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <GARGAS>
%%% @doc
%%%
%%% @end
%%% Created : 01. maj 2018 13:30
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Prezes").

%% API
-export([start/0, stop/0]).
-export([init/0]).
-export([crash/0, addValue/4, addStation/2, getOneValue/3,
  getAreaMean/3, getDailyMean/2, getStationMean/2, removeValue/3]).
-include("pollution_records.hrl").

start() -> register(pollutionServer, spawn_link(?MODULE, init, [])).


stop() -> pollutionServer ! stop.


init() -> loop(pollution:createMonitor()).


loop(Monitor) ->
  receive
    {request, Pid, addStation, {Name, {X, Y}}} ->
      R = pollution:addStation(Name, {X, Y}, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, ok}, loop(R)
      end;
    {request, Pid, addValue, {StationInfo, Date, Type, Value}} ->
      R = pollution:addValue(StationInfo, Date, Type, Value, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, ok}, loop(R)
      end;
    {request, Pid, removeValue, {StationInfo, Date, Type}} ->
      R = pollution:removeValue(StationInfo, Date, Type, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, ok}, loop(R)
      end;
    {request, Pid, getOneValue, {Type, Date, StationInfo}} ->
      R = pollution:getOneValue(Type, Date, StationInfo, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, R}, loop(Monitor)
      end;
    {request, Pid, getStationMean, {Type, StationInfo}} ->
      R = pollution:getStationMean(Type, StationInfo, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, R}, loop(Monitor)
      end;
    {request, Pid, getDailyMean, {Type, Date}} ->
      R = pollution:getDailyMean(Type, Date, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, R}, loop(Monitor)
      end;
    {request, Pid, getAreaMean, {StationInfo, Type, Radius}} ->
      R = pollution:getAreaMean(StationInfo, Type, Radius, Monitor),
      case R of
        {error, Msg} -> Pid ! {reply, Msg}, loop(Monitor);
        _ -> Pid ! {reply, R}, loop(Monitor)
      end;
    {request, Pid, stop} -> Pid ! {reply, ok}, unregister(pollutionServer);
    {request, Pid, crash} -> Pid ! {reply, ok}, 2/0
  end.


% Client API
call(Message, Arguments) ->
  pollutionServer ! {request, self(), Message, Arguments},
  receive
    {reply, Reply} -> Reply
  end.

addStation(Name, {X, Y}) -> call(addStation,{Name, {X, Y}}).
addValue(StationInfo, Date, Type, Value) -> call(addValue, {StationInfo, Date, Type, Value}).
removeValue(StationInfo, Date, Type) -> call(removeValue, {StationInfo, Date, Type}).
getOneValue(Type, Date, StationInfo) -> call(getOneValue, {Type, Date, StationInfo}).
getStationMean(Type, StationInfo) -> call(getStationMean, {Type, StationInfo}).
getDailyMean(Type, Date) -> call(getDailyMean, {Type, Date}).
getAreaMean(StationInfo, Type, Radius) -> call(getAreaMean, {StationInfo, Type, Radius}).
crash() -> pollutionServer ! crash.