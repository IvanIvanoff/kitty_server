defmodule KittyServerTest do
  use ExUnit.Case
  doctest KittyServer

  test "greets the world" do
    assert KittyServer.hello() == :world
  end
end
