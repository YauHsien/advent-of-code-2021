defmodule Aoc2021.Elixir do
  @moduledoc """
  Advent of Code, {:year, 2021}
  """

  @doc """
  Day1: sonar sweep of nearby sea floor

  ## Examination

      iex> Aoc2021.Elixir.do_day1_exercise("../inputs/day1_input.txt")
      1711

  """
  def do_day1_exercise(file_path) do
    stream =
      file_path
      |> File.stream!()
      |> Stream.map(fn elem -> Integer.parse(elem,10) end)
    [Stream.concat([:nil],stream), stream]
    |> Stream.zip_with(fn [{prev,"\n"},{elem,"\n"}] when prev < elem -> 1; _ -> 0 end)
    |> Enum.sum()
  end
end
