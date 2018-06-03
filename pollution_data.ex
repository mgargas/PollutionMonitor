defmodule PollutionData do
  @moduledoc false


  def importLinesFromCSV(file) do
    parsed_list = File.read!(file) |> String.split("\n") |> Enum.map(fn(a) -> parseLine(a) end)
    time1 = Benchmark.measure(fn() -> addStations(identifyStations(parsed_list)) end)
    time2 = Benchmark.measure(fn() -> addValues(parsed_list) end)
    IO.puts "Stations load: #{time1} s. Measurements load #{time2} s."
  end


  def parseLine(line) do
    [date, time, x, y, level] = String.split(line, ",")
    date = String.split(date, "-") |> Enum.reverse |> Enum.map(& Integer.parse /1 ) |> Enum.map(fn(a) -> elem(a, 0) end ) |> :erlang.list_to_tuple
    time = String.split(time, ":") |> Enum.map(& Integer.parse /1 ) |> Enum.map(fn(a) -> elem(a, 0) end ) |> :erlang.list_to_tuple
    x = elem(Float.parse(x), 0)
    y = elem(Float.parse(y), 0)
    level = elem(Integer.parse(level), 0)
    %{:datetime => {date, time}, :location => {x, y}, :pollutionLevel => level}
  end


  def identifyStations(measList) do
    locations = Enum.map(measList, fn(d) -> d.location end)
    MapSet.new(locations) |> MapSet.to_list()
    #Enum.reduce(measList, locations, fn(dict, locations) -> Map.put(locations, dict.location, "elo") end)
    #locations
  end


  def addStations(stationList) do
    get_name = fn x,y -> "station_#{x}_#{y}" end
    stations_to_add = Enum.map(stationList, fn {x,y} -> {get_name.(x, y), {x,y}} end)
    :pollution_server_supervisor.start_link()
    Enum.each(stations_to_add, fn {name, cords} -> :pollution_gen_server.addStation(name, cords) end)
  end


  def addValues(measList) do
    Enum.each(measList, fn m -> :pollution_gen_server.addValue(m.location, m.datetime, "PM10", m.pollutionLevel)  end)
  end
end
