defmodule KittyServerTest do
  use ExUnit.Case

  setup do
    kitty_server = KittyServer.start_link()
    {:ok, kitty_server: kitty_server}
  end

  test "kitty server serves kitties", %{kitty_server: kitty_server} do
    KittyServer.order_cat(kitty_server, "Rumen", "Grey", "Writes poetry")

    assert_receive %Kitty{color: "Grey", description: "Writes poetry", name: "Rumen"}, 5_000
  end
end
