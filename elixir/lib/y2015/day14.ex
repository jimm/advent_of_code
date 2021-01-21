# Reindeer Olympics

defmodule Y2015.Day14 do
  use Common.File

  @race_duration 2503
  @parse_regex ~r{(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.}

  defstruct [
    :name,
    :speed,
    :fly_duration,
    :rest_duration,
    :state,
    :time_in_state,
    :dist_travelled,
    :points
  ]

  # max dist
  def run1 do
    read_reindeer()
    |> Enum.map(&distance_at(&1, @race_duration))
    |> Enum.max()
  end

  def run1_test do
    read_reindeer(true)
    |> Enum.map(&distance_at(&1, 1000))
    |> Enum.max()
  end

  # wining points
  def run2 do
    read_reindeer()
    |> race(@race_duration - 1)
    |> Enum.map(& &1.points)
    |> IO.inspect()
    |> Enum.max()
  end

  defp read_reindeer(test \\ false) do
    if test do
      input_lines(default_input_path(__MODULE__, "_test"))
    else
      input_lines()
    end
    |> Enum.reduce([], fn line, acc ->
      [name, speed, dur, rest_duration] = parse(line)

      [
        %__MODULE__{
          name: name,
          speed: String.to_integer(speed),
          fly_duration: String.to_integer(dur),
          rest_duration: String.to_integer(rest_duration),
          state: :flying,
          time_in_state: 0,
          dist_travelled: 0,
          points: 0
        }
        | acc
      ]
    end)
  end

  defp parse(line) do
    Regex.run(@parse_regex, line) |> tl
  end

  # ================ method one ================

  defp distance_at(reindeer, secs) do
    distance_at(reindeer, 0, :fly, secs)
  end

  defp distance_at(reindeer, dist_travelled, :fly, secs) do
    if reindeer.fly_duration >= secs do
      dist_travelled + reindeer.speed * secs
    else
      distance_at(
        reindeer,
        dist_travelled + reindeer.speed * reindeer.fly_duration,
        :rest_duration,
        secs - reindeer.fly_duration
      )
    end
  end

  defp distance_at(reindeer, dist_travelled, :rest_duration, secs) do
    if reindeer.rest_duration >= secs do
      dist_travelled
    else
      distance_at(reindeer, dist_travelled, :fly, secs - reindeer.rest_duration)
    end
  end

  # ================ method two ================

  defp race(reindeer, 0), do: reindeer

  defp race(reindeer, secs) do
    reindeer = reindeer |> Enum.map(&update/1)
    max_dist = reindeer |> Enum.map(& &1.dist_travelled) |> Enum.max()
    reindeer = reindeer |> Enum.map(&give_points(&1, max_dist))
    race(reindeer, secs - 1)
  end

  defp update(%__MODULE__{state: :flying, time_in_state: t, fly_duration: t} = reindeer) do
    %{reindeer | state: :resting, time_in_state: 1}
  end

  defp update(%__MODULE__{state: :flying, time_in_state: t} = reindeer) do
    %{reindeer | dist_travelled: reindeer.dist_travelled + reindeer.speed, time_in_state: t + 1}
  end

  defp update(%__MODULE__{state: :resting, time_in_state: t, rest_duration: t} = reindeer) do
    %{
      reindeer
      | state: :flying,
        time_in_state: 1,
        dist_travelled: reindeer.dist_travelled + reindeer.speed
    }
  end

  defp update(%__MODULE__{state: :resting} = reindeer) do
    %{reindeer | time_in_state: reindeer.time_in_state + 1}
  end

  defp give_points(%__MODULE__{dist_travelled: d, points: p} = reindeer, max_dist)
       when d == max_dist do
    %{reindeer | points: p + 1}
  end

  defp give_points(reindeer, _), do: reindeer
end

# Y2015.Day14.max_dist
# # => 2653 WRONG, too low

# Y2015.Day14.winning_points
# # => 1102, 1635 is too high, 519 too low
