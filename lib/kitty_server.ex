defmodule KittyServer do
  @moduledoc """
  Documentation for KittyServer.
  """

  def start_link() do
    GenericServer.start_link(__MODULE__, [])
  end

  def init(state) do
    {:ok, state}
  end

  def order_kitty(pid, name, color, description) do
    GenericServer.call(pid, {:order, name, color, description})
  end

  def close_shop(pid) do
    GenericServer.stop(pid)
  end

  def return_kitty(pid, kitty) do
    GenericServer.cast(pid, {:return, kitty})
  end

  def handle_call({:order, name, color, description}, _from, []) do
    kitty = make_cat(name, color, description)
    {:reply, kitty, []}
  end

  def handle_call({:order, _, _, _}, _from, [kitty | rest_kitties]) do
    # If someone returns a cat, it's added to a list and is then automatically
    # sent as the next order instead of what the client actually asked for
    # (we're in this kitty store for the money, not smiles):
    {:reply, kitty, rest_kitties}
  end

  def handle_cast({:return, kitty}, state) do
    {:noreply, [kitty | state]}
  end

  def terminate(_reason, kitties) do
    kitties
    |> Enum.each(fn kitty -> IO.puts(kitty.name <> " was set free!!!") end)

    {:reply, :ok, []}
  end

  # Private functions

  defp make_cat(name, color, description) do
    %Kitty{name: name, color: color, description: description}
  end
end
