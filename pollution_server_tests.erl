%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <GARGAS>
%%% @doc
%%%
%%% @end
%%% Created : 01. maj 2018 17:46
%%%-------------------------------------------------------------------
-module(pollution_server_tests).
-author("Prezes").

-include_lib("eunit/include/eunit.hrl").
-include("pollution_records.hrl").

start_test() ->
  pollution_server:start(),
  ?assertNotEqual(undefined, whereis(pollutionServer)).


add_station_test() ->
  Current = pollution_server:addStation("Neverland", {14, 10}),
  ?assertEqual(Current, ok).


add_station_the_same_test() ->
  AddSamePlace = pollution_server:addStation("Central Perk", {14, 10}),
  ?assertNotEqual(AddSamePlace, ok).


add_value_test() ->
  D = calendar:local_time(),
  M = pollution_server:addValue({14, 10}, D, "X", 12),
  ?assertEqual(M, ok).


add_value_the_same_test() ->
  D = calendar:local_time(),
  Current = pollution_server:addStation("Long Island", {1, 1}),
  ?assertEqual(Current, ok),
  M = pollution_server:addValue({1, 1}, D, "X", 12),
  ?assertEqual(M, ok),
  N = pollution_server:addValue({1, 1}, D, "X", 15),
  ?assertNotEqual(N, ok).


add_value_without_station_test() ->
  D = calendar:local_time(),
  M = pollution_server:addValue({14, 10}, D, "X", 12),
  ?assertNotEqual(M, ok),
  N = pollution_server:addValue("Central Perk", D, "PM10", 15),
  ?assertNotEqual(N, ok).


remove_value_test() ->
  D = calendar:local_time(),
  M = pollution_server:addValue({14, 10}, D, "PM15", 20),
  ?assertEqual(M, ok),
  N = pollution_server:removeValue({14, 10}, D, "PM15"),
  ?assertEqual(N, ok).


get_one_value_test() ->
  D = calendar:local_time(),
  ?assert(pollution_server:getOneValue("X", D, {14, 10}) =:= 12).


get_station_mean_test() ->
  D = calendar:local_time(),
  Current = pollution_server:addStation("Eastchester", {99, 99}),
  ?assertEqual(Current, ok),
  M = pollution_server:addValue({99, 99}, D, "X10", 12),
  ?assertEqual(M, ok),
  N = pollution_server:addValue({99, 99}, calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}), "X10", 16),
  ?assertEqual(N, ok),
  ?assert(pollution_server:getStationMean("X10", "Eastchester") == 14).


get_daily_mean_test() ->
  D = calendar:local_time(),
  M = pollution_server:addStation("Lodolamacz Moskwa", {15, 10}),
  ?assertEqual(M, ok),
  N = pollution_server:addStation("Lodolamacz Kijow", {19, 97}),
  ?assertEqual(N, ok),
  K = pollution_server:addValue({15, 10}, D, "CO2", 12),
  ?assertEqual(K, ok),
  L = pollution_server:addValue({19, 97}, D, "CO2", 10),
  ?assertEqual(L, ok),
  ?assert(pollution_server:getDailyMean("CO2", element(1, D)) == 11).


get_area_mean_test() ->
  D = calendar:local_time(),
  T = calendar:universal_time_to_local_time({{2014, 7, 10}, {20, 29, 12}}),
  M = pollution_server:addStation("Neverland2", {10, 10}),
  ?assertEqual(M, ok),
  N = pollution_server:addStation("Camp Nou", {10, 11}),
  ?assertEqual(N, ok),
  K = pollution_server:addValue("Neverland2", D, "X", 12),
  ?assertEqual(K, ok),
  L = pollution_server:addValue("Neverland2", T, "X", 8),
  ?assertEqual(L, ok),
  P = pollution_server:addValue("Camp Nou", D, "X", 35),
  ?assertEqual(P, ok),
  R = pollution_server:addValue("Camp Nou", T, "X", 45),
  ?assertEqual(R, ok),
  ?assert(pollution_server:getAreaMean({10, 10}, "X", 2) == 25).