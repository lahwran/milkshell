defmodule MilkshellTest do
  use ExUnit.Case
  doctest Milkshell

  test "greets the world" do
    assert Milkshell.hello() == :world
  end
end
