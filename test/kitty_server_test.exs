defmodule KittyServerTest do
  use ExUnit.Case

  setup do
    kitty_server = KittyServer.start_link()
    {:ok, kitty_server: kitty_server}
  end

  test "kitty server serves kitties", %{kitty_server: kitty_server} do
    expected_kitty = %Kitty{color: "Grey", description: "Writes poetry", name: "Rumen"}

    received_kitty = KittyServer.order_kitty(kitty_server, "Rumen", "Grey", "Writes poetry")

    assert expected_kitty == received_kitty
  end

  test "catches when a kitty store dies", %{kitty_server: kitty_server} do
    :ok = KittyServer.close_shop(kitty_server)
    {:error, msg} = KittyServer.order_kitty(kitty_server, "Rumen", "Grey", "Writes poetry")
    assert msg =~ "closed"
  end

  test "return works as not expected by the average customer", %{kitty_server: kitty_server} do
    kitty = KittyServer.order_kitty(kitty_server, "Rumen", "Grey", "Writes poetry")
    :ok = KittyServer.return_kitty(kitty_server, kitty)
    kitty2 = KittyServer.order_kitty(kitty_server, "Tommy", "Red", "untalanted")

    # You got Rumen
    assert kitty == kitty2
  end
end
