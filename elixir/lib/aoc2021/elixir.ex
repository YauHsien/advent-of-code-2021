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


  @doc """
  Day2: Dive!

  ## Examination

      iex> Aoc2021.Elixir.do_day2_exercise("../inputs/day2_input.txt")
      1868935
  """
  def do_day2_exercise(file_path) do
    file_path
    |> File.stream!()
    |> parse_command()
    |> operate_submarine!()
  end

  @spec parse_command(stream :: File.Stream.t()) :: (commands :: Stream.t())
  defp parse_command(stream) do
    stream
    |> Stream.map(fn line -> [command, raw] = String.split(line, [" ","\n"], trim: true)
      {command, String.to_integer(raw)}
    end)
  end

  @spec operate_submarine(commands :: Stream.t(), position :: Keyword.t()) :: (matter :: Keyword.t())
  defp operate_submarine(commands, position \\ [hposition: 0, depth: 0]) do
    commands
    |> Enum.reduce(position, fn {"forward",number}, acc -> Keyword.put(acc, :hposition, acc[:hposition]+number);
    {"down",number}, acc -> Keyword.put(acc, :depth, acc[:depth]+number);
      {"up",number}, acc -> Keyword.put(acc, :depth, acc[:depth]-number)
    end)
  end

  @spec operate_submarine!(stream :: Stream.t()) :: (result :: Integer.t())
  defp operate_submarine!(stream) do
    matter =
      stream
      |> operate_submarine()
    matter[:hposition] * matter[:depth]
  end

  @doc """
  Day2/Part2: dive by aiming.

  ## Examination

      iex> Stream.concat([[{"forward",5}]])
      ...> |> Aoc2021.Elixir.conclude_dive!() == 5*0
      true

      iex> Stream.concat([[{"forward",5},{"down",5}]])
      ...> |> Aoc2021.Elixir.conclude_dive!() == 5*0
      true

      iex> Stream.concat([[{"forward",5},{"down",5},{"forward",8}]])
      ...> |> Aoc2021.Elixir.conclude_dive!() == (5+8)*(8*5)
      true

      iex> Stream.concat([[{"forward",5},{"down",5},{"forward",8},{"up",3}]])
      ...> |> Aoc2021.Elixir.conclude_dive!() == (5+8)*(8*5)
      true

      iex> Stream.concat([[{"forward",5},{"down",5},{"forward",8},{"up",3},{"down",8}]])
      ...> |> Aoc2021.Elixir.conclude_dive!() == (5+8)*(8*5)
      true

      iex> Stream.concat([[{"forward",5},{"down",5},{"forward",8},{"up",3},{"down",8},{"forward",2}]])
      ...> |> Aoc2021.Elixir.conclude_dive!() == (5+8+2)*(8*5+2*10)
      true

      iex> Aoc2021.Elixir.do_day2_part2_exercise("../inputs/day2_input.txt")
      1965970888
  """
  def do_day2_part2_exercise(file_path) do
    file_path
    |> File.stream!()
    |> parse_command()
    |> conclude_dive!()
  end

  def conclude_dive!(commands) do
    commands
    |> operate_submarine_by_aiming()
    |> conclude_dive()
  end

  @spec operate_submarine_by_aiming(commands :: Stream.t(), position :: Keyword.t()) :: (matter :: Keyword.t())
  defp operate_submarine_by_aiming(commands, position \\ [hposition: 0, depth: 0, aim: 0]) do
    commands
    |> Enum.reduce(position, fn {"forward",number}, acc ->
      acc
      |> Keyword.put(:hposition, acc[:hposition]+number)
      |> Keyword.put(:depth,     acc[:depth]+number*acc[:aim]);
    {"down",number}, acc -> Keyword.put(acc, :aim, acc[:aim]+number);
      {"up",number}, acc -> Keyword.put(acc, :aim, acc[:aim]-number)
    end)
  end

  @spec conclude_dive(matter :: Keyword.t()) :: (result :: Integer.t())
  defp conclude_dive(matter) do
    matter[:hposition] * matter[:depth]
  end

  @spec parse_line(stream :: File.Stream.t()) :: (lines :: Stream.t())
  defp parse_line(stream) do
    stream
    |> Stream.map(fn line -> [line1] = String.split(line, ["\n"], trim: true)
      line1
    end)
  end

  @doc """
  ## Examination

      iex> Aoc2021.Elixir.do_day3_exercise("../inputs/day3_input.txt")
      3958484
  """
  def do_day3_exercise(file_path) do
    file_path
    |> File.stream!()
    |> parse_line()
    |> Stream.map(&String.to_charlist/1)
    |> Stream.zip_with(&binary_diagnostic/1)
    |> Stream.zip_with(&(&1 |> List.flatten() |> List.to_integer(2)))
    |> Enum.to_list()
    |> List.foldl(1, &*/2)
  end

  defp binary_diagnostic(list) do
    {first, second} = {Enum.count(list), Enum.count(list,&([&1]=='0'))}
    if(first-second > second, do: ['1','0'], else: ['0','1'])
  end

  @doc """
  ## Examination

      iex> Aoc2021.Elixir.do_day3_part2_exercise("../inputs/day3_input.txt")
      1613181
  """
  def do_day3_part2_exercise(file_path) do
    file_path
    |> File.stream!()
    |> parse_line()
    |> Stream.map(&String.to_charlist/1)
    |> init_oxygen_CO2_profile()
    |> find_oxygen_CO2_ratings()
    |> conclude_oxygen_CO2_profile()
    |> Kernel.then(&([
          &1[:o] |> List.first() |> List.to_integer(2),
          &1[:c] |> List.first() |> List.to_integer(2)
        ]))
    |> Kernel.then(&(List.first(&1)*List.last(&1)))
  end

  @spec init_oxygen_CO2_profile(Stream.t()) :: [{ :iter, number()} | { :o|:c, Stream.t() }]
  defp init_oxygen_CO2_profile(stream) do
    [ iter: 2,
      o: stream |> Stream.filter(&([List.first(&1)]=='1')),
      c: stream |> Stream.filter(&([List.first(&1)]=='0'))
    ]
  end

  defp find_oxygen_CO2_ratings([iter: i, o: o, c: c] = profile, iter \\ 12) do
    lo = o |> Enum.count()
    lc = c |> Enum.count()
    if i > iter or (lo == 1 and lc == 1) do
      profile
    else
      [ iter: i+1,
        o: if(i > iter or lo == 1, do: o, else: separate_by_common(i,o,:most)),
        c: if(i > iter or lc == 1, do: c, else: separate_by_common(i,c,:least))
      ]
      |> find_oxygen_CO2_ratings()
    end
  end

  @spec separate_by_common(Integer.i(), Stream.t(), :most | :least) :: Stream.t()
  defp separate_by_common(i, stream, common_by) do
    {o, c} = {
      stream |> Stream.filter(&([Enum.at(&1,i-1)]=='1')),
      stream |> Stream.filter(&([Enum.at(&1,i-1)]=='0')),
    }
    {lo, lc} = {
      o |> Enum.count(),
      c |> Enum.count()
    }
    case common_by do
      :most ->
        if lo == lc do
          o
        else
          if(lo > lc, do: o, else: c)
        end
      :least ->
        if lo == lc do
          c
        else
          if(lo > lc, do: c, else: o)
        end
    end
  end

  defp conclude_oxygen_CO2_profile([iter: i, o: o, c: c]) do
    [ iter: i,
      lo: o |> Enum.count(),
      lc: c |> Enum.count(),
      o: o |> Enum.to_list(),
      c: c |> Enum.to_list()
    ]
  end
end
