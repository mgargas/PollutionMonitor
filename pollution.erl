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
  getOneValue/4, getStationMean/3, getDailyMean/3, getAreaMean/4, getStation/2]).
-include("pollution_records.hrl").


createMonitor() -> #monitor{}.


addStation(Name, {X,Y}, Monitor) ->
  case dict:is_key(Name, (Monitor#monitor.stats)#stations.name_to_elem)
  or dict:is_key({X, Y}, (Monitor#monitor.stats)#stations.coord_to_elem) of
    true -> {error, "Station already exists"};
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
  case getStation(StationInfo, Monitor) of
    {error, Msg} -> {error, Msg};
    Station ->
      case dict:is_key({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas) of
        true -> {error, "Measurement that has this type, date and station already exists"};
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
      end
  end.


removeValue(StationInfo, Date, Type, Monitor) ->
  case getStation(StationInfo, Monitor) of
    {error, Msg} -> {error, Msg};
    Station ->
      case dict:is_key({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas) of
        false -> {error, "Measurement that has this type, date and station doesn't exist"};
        true ->
          Measurement = dict:fetch({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
          Meas = (Monitor#monitor.meas),
          Station = getStation(StationInfo, Monitor),
          {Old1, TypeDateToMeas} = dict:take({Type, element(1, Date)}, Meas#measurements.type_date_to_meas),
          {Old2, TypeStationToMeas} = dict:take({Type, Station}, Meas#measurements.type_station_to_meas),
          NewMeas = Meas#measurements{
            all = sets:del_element(Measurement, Meas#measurements.all),
            type_date_station_to_meas = dict:erase({Type, Date, Station}, Meas#measurements.type_date_station_to_meas),
            type_date_to_meas = dict:append_list({Type, Date}, lists:filter(fun(X) -> X =/= Measurement end, Old1), TypeDateToMeas),
            type_station_to_meas = dict:append_list({Type, Station}, lists:filter(fun(X) -> X =/= Measurement end, Old2), TypeStationToMeas)
          },
          Monitor#monitor{meas = NewMeas}
      end
  end.


getOneValue(Type, Date, StationInfo, Monitor) ->
  case getStation(StationInfo, Monitor) of
    {error, Msg} -> {error, Msg};
    Station ->
      case dict:is_key({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas) of
        false -> {error, "Measurement that has this type, date and station doesn't exist"};
        true ->
          {_, _, _, V} = dict:fetch({Type, Date, Station}, (Monitor#monitor.meas)#measurements.type_date_station_to_meas),
          V
      end
  end.


getStationMean(Type, StationInfo, Monitor) ->
  case getStation(StationInfo, Monitor) of
    {error, Msg} -> {error, Msg};
    Station ->
      case dict:is_key({Type, Station}, (Monitor#monitor.meas)#measurements.type_station_to_meas) of
        false -> {error, "Measurement that has this type and station doesn't exist"};
        true ->
          Measurements = dict:fetch({Type, Station}, (Monitor#monitor.meas)#measurements.type_station_to_meas),
          average(Measurements)
      end
  end.


getDailyMean(Type, Date, Monitor) ->
  case dict:is_key({Type, Date}, (Monitor#monitor.meas)#measurements.type_date_to_meas) of
    false -> {error, "Measurement that has this type and date doesn't exist"};
    true ->
      Measurements = dict:fetch({Type, Date}, (Monitor#monitor.meas)#measurements.type_date_to_meas),
      average(Measurements)
  end.


getAreaMean(StationInfo, Type, Radius, Monitor) ->
  case getStation(StationInfo, Monitor) of
    {error, Msg} -> {error, Msg};
    {_, {X, Y}} ->
      Predicate = fun({T, {_, {XS, YS}}}, _) -> (T =:= Type) and (math:pow(XS - X, 2) + math:pow(YS - Y, 2) =< math:pow(Radius, 2)) end,
      NewDict = dict:filter(Predicate, (Monitor#monitor.meas)#measurements.type_station_to_meas),
      average(lists:flatten(dict:fold(fun(_, V, AccIn) -> [V|AccIn] end, [], NewDict)))
  end.


average(L) -> average(L, 0, 0).
average([{_, _, _, V}|T], Length, Sum) -> average(T, Length + 1, Sum + V);
average([], Length, Sum) -> Sum/Length.


getStation({X,Y}, Monitor) -> case dict:is_key({X, Y}, (Monitor#monitor.stats)#stations.coord_to_elem) of
                                  false -> {error, "Station having these coordinates doesn't exist"};
                                  true -> dict:fetch({X,Y}, (Monitor#monitor.stats)#stations.coord_to_elem)
                              end;
getStation(Name, Monitor) -> case dict:is_key(Name, (Monitor#monitor.stats)#stations.name_to_elem) of
                                  false -> {error, "Station having this name doesn't exist"};
                                  true -> dict:fetch(Name, (Monitor#monitor.stats)#stations.name_to_elem)
                             end.



