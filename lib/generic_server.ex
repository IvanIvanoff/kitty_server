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
    send(pid, {:call, self(), ref, msg})

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

  def loop(module, state) do
    receive do
      {:call, pid, ref, :terminate} ->
        IO.inspect( state)
        case module.terminate(state) do
          {:reply, result, _state} ->
            send(pid, {ref, result})
            :ok
        end

      {:call, pid, ref, msg} ->
        case module.handle_call(msg, pid, ref, state) do
          {:reply, result, new_state} ->
            send(pid, {ref, result})
            loop(module, new_state)
        end

      {:cast, msg} ->
        {:noreply, new_state} = module.handle_cast(msg, state)
        loop(module, new_state)
    end
  end
end
