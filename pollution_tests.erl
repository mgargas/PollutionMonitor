%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <GARGAS>
%%% @doc
%%%
%%% @end
%%% Created : 14. kwi 2018 04:12
%%%-------------------------------------------------------------------
-module(pollution_tests).
-author("Prezes").
-include_lib("eunit/include/eunit.hrl").

-record(measurements, {all = sets:new(),
  type_date_station_to_meas = dict:new(),
  type_date_to_meas = dict:new(),
  type_station_to_meas = dict:new()}).
-record(stations, {all = sets:new(), coord_to_elem = dict:new(), name_to_elem = dict:new()}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).


create_monitor_test() ->
  Current = pollution:createMonitor(),
  Expected = {
    monitor,
    {measurements,
      sets:new(),
      dict:new(),
      dict:new(),
      dict:new()
    },
    {stations,
      sets:new(),
      dict:new(),
      dict:new()
    }
  },
  ?assertEqual(Current, Expected).


add_station_test() ->
  Monitor = pollution:createMonitor(),
  Current = pollution:addStation("Neverland", {14, 10}, Monitor),
  ?assert(sets:is_element({"Neverland", {14, 10}}, (Current#monitor.stats)#stations.all)),
  ?assert(dict:fetch("Neverland",
    (Current#monitor.stats)#stations.name_to_elem) =:= {"Neverland", {14, 10}}),
  ?assert(dict:fetch({14, 10},
    (Current#monitor.stats)#stations.coord_to_elem) =:= {"Neverland", {14, 10}}).


add_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorWithS = pollution:addStation("Neverland", {14, 10}, Monitor),
  Current = pollution:addValue({14, 10}, D, "X", 12, MonitorWithS),
  ?assert(sets:is_element({{"Neverland", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)),
  ?assert(element(2, dict:find({"X", element(1,D)},
    (Current#monitor.meas)#measurements.type_date_to_meas)) =:= [{{"Neverland", {14, 10}}, D, "X", 12}]).


remove_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorWithS = pollution:addStation("Neverland", {14, 10}, Monitor),
  CurrentButNotRemoved = pollution:addValue({14, 10}, D, "X", 12, MonitorWithS),
  Current = pollution:removeValue({14, 10}, D, "X", CurrentButNotRemoved),
  ?assertNot(sets:is_element({{"Neverland", {14, 10}}, D, "X", 12}, (Current#monitor.meas)#measurements.all)),
  ?assert(element(2, dict:find({"X", D}, (Current#monitor.meas)#measurements.type_date_to_meas)) == []),
  ?assert(element(2, dict:find({"X", {"Neverland", {14, 10}}}, (Current#monitor.meas)#measurements.type_station_to_meas)) == []).


get_one_value_test() ->
  D = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorWithS = pollution:addStation("Neverland", {14, 10}, Monitor),
  Current = pollution:addValue({14, 10}, D, "X", 12, MonitorWithS),
  ?assert(pollution:getOneValue("X", D, {14, 10}, Current) =:= 12).

get_station_mean_test() ->
  D = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorWithS = pollution:addStation("Neverland", {14, 10}, Monitor),
  Current = pollution:addValue({14, 10}, D, "X", 12, MonitorWithS),
  Current2 = pollution:addValue({14, 10}, calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}), "X", 10, Current),
  ?assert(pollution:getStationMean("X", "Neverland", Current2) == 11).


get_daily_mean_test() ->
  D = calendar:local_time(),
  Monitor = pollution:createMonitor(),
  MonitorWithS = pollution:addStation("Lodolamacz Moskwa", {14, 10}, Monitor),
  MonitorWithS2 = pollution:addStation("Lodolamacz Kijow", {19, 97}, MonitorWithS),
  Current = pollution:addValue({14, 10}, D, "X", 12, MonitorWithS2),
  Current2 = pollution:addValue({19, 97}, D, "X", 10, Current),
  ?assert(pollution:getDailyMean("X", element(1, D), Current2) == 11).


get_area_mean_test() ->
  D = calendar:local_time(),
  T = calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}),
  Monitor = pollution:createMonitor(),
  MonitorWithS = pollution:addStation("Neverland", {10, 10}, Monitor),
  MonitorWithS2 = pollution:addStation("Camp Nou", {10, 11}, MonitorWithS),
  Current = pollution:addValue("Neverland", D, "X", 12, MonitorWithS2),
  Current2 = pollution:addValue("Neverland", T, "X", 8, Current),
  Current3 = pollution:addValue("Camp Nou", D, "X", 35, Current2),
  Current4 = pollution:addValue("Camp Nou", T, "X", 45, Current3),
  ?assert(pollution:getAreaMean({10, 10}, "X", 4, Current4) == 25).