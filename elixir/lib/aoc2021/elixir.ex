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
      file_path
      |> File.stream!()
      |> Stream.map(fn elem -> Integer.parse(elem,10) end)
      |> count_increased_numbers!()
  end

  @spec count_increased_numbers!(stream :: Stream.t()) :: (count :: Integer.t())
  defp count_increased_numbers!(stream) do
    [Stream.concat([:nil],stream), stream]
    |> Stream.zip_with(fn [{prev,"\n"},{elem,"\n"}] when prev < elem -> 1;
      _ -> 0 end)
    |> Enum.sum()
  end

  @doc """
  Day1/Part2: sonar sweep of nearby sea floor

  ## Examination

      iex> Aoc2021.Elixir.do_day1_part2_exercise("../inputs/day1_input.txt")
      1743

  """
  def do_day1_part2_exercise(file_path) do
    stream =
      file_path
      |> File.stream!()
      |> Stream.map(fn elem -> Integer.parse(elem,10) end)
      three_sum_list =
        [Stream.concat([:nil,:nil],stream), Stream.concat([:nil],stream), stream]
        |> Stream.zip_with(fn [:nil,:nil,{_third,"\n"}] -> :nil;
        [:nil,{_second,"\n"},{_third,"\n"}] -> :nil;
        [{first,"\n"},{second,"\n"},{third,"\n"}] -> {first+second+third,"\n"}
      end)
        three_sum_list
        |> count_increased_numbers!()
  end
end
