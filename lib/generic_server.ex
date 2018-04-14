defmodule GenericServer do
  def start_link(module, state) do
    spawn_link(fn -> init(module, state) end)
  end

  def cast(pid, msg) do
    send(pid, {:cast, msg})
    :ok
  end

  def call(pid, msg) do
    ref = Process.monitor(pid)
    send(pid, {:call, {self(), ref}, msg})

    receive do
      {^ref, reply} ->
        Process.demonitor(ref)
        reply

      {:DOWN, ^ref, :process, ^pid, status} ->
        {:error, "Kitty store closed abruptly with status #{status}."}

      msg ->
        IO.warn("Unexpected messgage received: #{inspect(msg)}")
    end
  end

  # Private functions
  defp init(module, state) do
    loop(module, module.init(state))
  end

  defp loop(module, state) do
    receive do
      {:call, from, :terminate} ->
        case module.terminate(state) do
          {:reply, result, _state} ->
            reply(from, result)
            :ok
        end

      {:call, from, msg} ->
        case module.handle_call(msg, from, state) do
          {:reply, result, new_state} ->
            reply(from, result)
            loop(module, new_state)
        end

      {:cast, msg} ->
        {:noreply, new_state} = module.handle_cast(msg, state)
        loop(module, new_state)
    end
  end

  defp reply({pid, ref}, msg) do
    send(pid, {ref, msg})
  end
end
