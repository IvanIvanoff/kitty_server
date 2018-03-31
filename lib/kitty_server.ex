defmodule KittyServer do
  @moduledoc """
  Documentation for KittyServer.
  """

  def start_link() do
    spawn_link(&init/0)
  end

  def order_cat(pid, name, color, description) do
    send(pid, {self(), {:order, name, color, description}})
  end

  # Private functions

  defp loop(kitties) do
    receive do
      {pid, {:order, name, color, description}} ->
        case kitties do
          [] ->
            send(pid, make_cat(name, color, description))
            loop(kitties)

          # As stated by Fred in his book:
          # If someone returns a cat, it's added to a list and is then automatically
          # sent as the next order instead of what the client actually asked for
          # (we're in this kitty store for the money, not smiles):
          [kitty | rest_kitties] ->
            send(pid, kitty)
            loop(rest_kitties)
        end

      {:return, kitty = %Kitty{}} ->
        loop([kitty | kitties])

      {pid, :terminate} ->
        send(pid, :ok)
        terminate(kitties)

      unknown ->
        IO.warn("Unknown message received: #{inspect(unknown)}")
        loop(kitties)
    end
  end

  defp init() do
    loop([])
  end

  defp make_cat(name, color, description) do
    %Kitty{name: name, color: color, description: description}
  end

  defp terminate(kitties) do
    kitties
    |> Enum.each(fn kitty -> IO.puts(kitty.name <> " was set free!!!") end)

    :ok
  end
end
