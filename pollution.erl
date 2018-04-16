%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <GARGAS S.A>
%%% @doc
%%%
%%% @end
%%% Created : 11. kwi 2018 23:58
%%%-------------------------------------------------------------------
-module(pollution).
-author("Prezes").
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4,
  getOneValue/4, getStationMean/3, getDailyMean/3, getAreaMean/4]).


-record(measurements, {all = sets:new(),
  type_date_station_to_meas = dict:new(),
  type_date_to_meas = dict:new(),
  type_station_to_meas = dict:new()}).
-record(stations, {all = sets:new(), coord_to_elem = dict:new(), name_to_elem = dict:new()}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).


createMonitor() -> #monitor{}.


addStation(Name, {X,Y}, Monitor) ->
  case dict:is_key(Name, (Monitor#monitor.stats)#stations.name_to_elem)
  or dict:is_key({X, Y}, (Monitor#monitor.stats)#stations.coord_to_elem) of
    true -> error(badarg);
    false ->
  S = {Name, {X,Y}},
  Stats = (Monitor#monitor.stats),
  NewStats = Stats#stations
  {all = sets:add_element(S, Stats#stations.all),
    coord_to_elem = dict:store({X, Y}, S, Stats#stations.coord_to_elem),
    name_to_elem = dict:store(Name, S, Stats#stations.name_to_elem)
  },
  Monitor#monitor{stats = NewStats}
  end.


addValue(StationInfo, Date, Type, Value, Monitor) ->
  Station = getStation(StationInfo, Monitor),
  case dict:is_key({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas) of
    true -> error(badarg);
    false ->
  Measurement = {Station, Date, Type, Value},
  Meas = (Monitor#monitor.meas),
  NewMeas = Meas#measurements{
    all = sets:add_element(Measurement, Meas#measurements.all),
    type_date_station_to_meas = dict:store({Type, Date, Station}, Measurement, Meas#measurements.type_date_station_to_meas),
    type_date_to_meas = dict:append({Type, element(1, Date)}, Measurement, Meas#measurements.type_date_to_meas),
    type_station_to_meas = dict:append({Type, Station}, Measurement, Meas#measurements.type_station_to_meas)
  },
  Monitor#monitor{meas = NewMeas}
  end.


removeValue(StationInfo, Date, Type, Monitor) ->
  Station = getStation(StationInfo, Monitor),
  Meas = (Monitor#monitor.meas),
  Measurement = dict:fetch({Type, Date, Station}, Meas#measurements.type_date_station_to_meas),
  {Old1, TypeDateToMeas} = dict:take({Type, element(1, Date)}, Meas#measurements.type_date_to_meas),
  {Old2, TypeStationToMeas} = dict:take({Type, Station}, Meas#measurements.type_station_to_meas),
  NewMeas = Meas#measurements{
    all = sets:del_element(Measurement, Meas#measurements.all),
    type_date_station_to_meas = dict:erase({Type, Date, Station}, Meas#measurements.type_date_station_to_meas),
    type_date_to_meas = dict:append_list({Type, Date}, lists:filter(fun(X) -> X =/= Measurement end, Old1), TypeDateToMeas),
    type_station_to_meas = dict:append_list({Type, Station}, lists:filter(fun(X) -> X =/= Measurement end, Old2), TypeStationToMeas)
  },
  Monitor#monitor{meas = NewMeas}.


getOneValue(Type, Date, StationInfo, Monitor) ->
  Station = getStation(StationInfo, Monitor),
  {_, _, _, V} = dict:fetch({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
  V.


getStationMean(Type, StationInfo, Monitor) ->
  Station = getStation(StationInfo, Monitor),
  Measurements = dict:fetch({Type, Station}, (Monitor#monitor.meas)#measurements.type_station_to_meas),
  average(Measurements).

getDailyMean(Type, Date, Monitor) ->
  Measurements = dict:fetch({Type, Date}, (Monitor#monitor.meas)#measurements.type_date_to_meas),
  average(Measurements).


getAreaMean(StationInfo, Type, Radius, Monitor) ->
  {_, {X, Y}} = getStation(StationInfo, Monitor),
  Predicate = fun({T, {_, {XS, YS}}}, _) -> (T =:= Type) and (math:pow(XS - X, 2) + math:pow(YS - Y, 2) =< math:pow(Radius, 2)) end,
  NewDict = dict:filter(Predicate, (Monitor#monitor.meas)#measurements.type_station_to_meas),
  average(lists:flatten(dict:fold(fun(_, V, AccIn) -> [V|AccIn] end, [], NewDict))).


average(L) -> average(L, 0, 0).
average([{_, _, _, V}|T], Length, Sum) -> average(T, Length + 1, Sum + V);
average([], Length, Sum) -> Sum/Length.


getStation({X,Y}, Monitor) -> dict:fetch({X,Y}, (Monitor#monitor.stats)#stations.coord_to_elem);
getStation(Name, Monitor) -> dict:fetch(Name, (Monitor#monitor.stats)#stations.name_to_elem).



