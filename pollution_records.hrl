%%%-------------------------------------------------------------------
%%% @author Prezes
%%% @copyright (C) 2018, <GARGAS>
%%% @doc
%%%
%%% @end
%%% Created : 01. maj 2018 13:22
%%%-------------------------------------------------------------------
-author("Prezes").

-record(measurements, {all = sets:new(),
  type_date_station_to_meas = dict:new(),
  type_date_to_meas = dict:new(),
  type_station_to_meas = dict:new()}).
-record(stations, {all = sets:new(), coord_to_elem = dict:new(), name_to_elem = dict:new()}).
-record(monitor, {meas = #measurements{}, stats = #stations{}}).