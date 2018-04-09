defmodule GenericServer do
  def call(pid, msg) do
    ref = Process.monitor(pid)
    send(pid, {self(), ref, msg})

    receive do
      {^ref, reply} ->
        Process.demonitor(ref)
        reply

      {:DOWN, ^ref, :process, ^pid, status} ->
        {:error, "Kitty store closed abruptly with status #{status}."}
    end
  end
end