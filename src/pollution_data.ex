defmodule PollutionData do

  def convert_date(date) do
    date
    |> String.split("-")
    |> Enum.reverse
    |> Enum.map(&(elem(Integer.parse(&1), 0)))
    |> :erlang.list_to_tuple
  end

  def convert_time(time) do
    time
    |> String.split(":")
    |> Enum.map(&(elem(Integer.parse(&1), 0)))
    |> :lists.append([0])
    |> :erlang.list_to_tuple
  end

  def convert_location(loc_x, loc_y) do
    [loc_x, loc_y]
    |> Enum.map(&(elem(Float.parse(&1), 0)))
    |> :erlang.list_to_tuple
  end

  def convert_line(line) do
    [date, time, loc_x, loc_y, value] = String.split(line, ",")

    {value, _} = Float.parse(value)

    %{
      datetime: {convert_date(date), convert_time(time)},
      location: convert_location(loc_x, loc_y),
      pollutionLevel: value
    }
  end

  def convert_lines(lines) do
    lines
    |> Enum.map(&(convert_line(&1)))
  end

  def import_lines_from_csv(file) do
    file
    |> File.read!
    |> String.split("\r\n")
    |> convert_lines
  end

  def identify_stations(data) do
    data
    |> Enum.map(fn (%{location: loc}) -> loc end)
    |> Enum.uniq
  end

  def add_stations(data) do
    data
    |> identify_stations
    |> Enum.map(
         fn ({locx, locy}) ->
           {"station_#{locx}_#{locy}", {locx, locy}}
         end
       )
    |> Enum.each(
         fn ({station, location}) ->
           :pollution_gen_server.add_station(station, location)
         end
       )
  end

  def add_measurements(data) do
    data
    |> Enum.each(
         fn (%{location: id, datetime: datetime, pollutionLevel: value}) ->
           :pollution_gen_server.add_value(id, datetime, "PM10", value)
         end
       )
  end

end
