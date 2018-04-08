defmodule KittyServer do
  @moduledoc """
  Documentation for KittyServer.
  """

  def start_link() do
    spawn_link(&init/0)
  end

  def order_kitty(pid, name, color, description) do
    ref = Process.monitor(pid)
    send(pid, {self(), {:order, name, color, description}})

    receive do
      {:ok, ^ref, kitty} ->
        Process.demonitor(ref)
        {:ok, kitty}

      {:DOWN, ^ref, :process, ^pid, status} ->
        {:error, "Kitty store closed abruptly with status #{status}."}
    end
  end

  def close_shop(pid) do
    ref = Process.monitor(pid)
    send(pid, {self(), :terminate})

    receive do
      :ok ->
        Process.demonitor(ref)
        {:ok, :ok}

      {:DOWN, ^ref, :process, ^pid, status} ->
        {:error, "Kitty store closed abruptly with status #{status}."}
    end
  end

  def return_kitty(pid, kitty) do
    ref = Process.monitor(pid)
    send(pid, {:return, kitty})

    :ok
  end

  # Private functions

  defp loop(kitties) do
    receive do
      {pid, ref, {:order, name, color, description}} ->
        case kitties do
          [] ->
            send(pid, {:ok, make_cat(name, color, description)})
            loop(kitties)

          # If someone returns a cat, it's added to a list and is then automatically
          # sent as the next order instead of what the client actually asked for
          # (we're in this kitty store for the money, not smiles):
          [kitty | rest_kitties] ->
            send(pid, {:ok, ref, kitty})
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
    after
      60_000 -> raise("No one wants kitties")
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
