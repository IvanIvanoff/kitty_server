defmodule GenericServer do
  def start_link(module, state) do
    {:ok, spawn_link(fn -> init(module, state) end)}
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
        Process.demonitor(ref, [:flush])
        reply

      {:DOWN, ^ref, :process, ^pid, status} ->
        exit(status)
    end
  end

  def stop(pid) do
    send(pid, :terminate)
    :ok
  end

  # Private functions
  defp init(module, state) do
    case module.init(state) do
      {:ok, initial_state} ->
        loop(module, initial_state)

      {:error, reason} ->
        raise(reason)
    end
  end

  defp loop(module, state) do
    receive do
      :terminate ->
        module.terminate(:shutdown, state)
        :ok

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
