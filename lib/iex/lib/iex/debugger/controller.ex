defmodule IEx.Debugger.Controller do
  use GenServer.Behaviour

  alias IEx.Debugger.Runner

  @server_name { :global, :controller }

  defrecord State, [client: nil, processes: nil, patterns: []]

  def alive? do
    { _reg, name } = @server_name
    :global.whereis_name(name) != :undefined
  end

  def start_link(opts) do
    if alive?, do: :gen_server.call(@server_name, :stop)
    :gen_server.start_link(@server_name, __MODULE__, opts, [])
  end

  def init(client_pid) do
    { :ok, State[client: client_pid, processes: HashDict.new] }
  end

  def handle_call(:stop, _sender, dict) do
    { :stop, :normal, :shutdown_ok, dict }
  end
  
  def handle_call(:list, _sender, state) do
    { :reply, state.processes, state }
  end

  # The Controller keeps track of the processes 
  # currently running, being notified through next.
  def handle_cast({ :next, pid, expr }, state) do
    if state.client do
      state.client <- { :debug, { :next, pid, expr }}
    end
    step(pid)
    { :noreply, state.processes(Dict.put(state.processes, pid, expr)) }
  end

  def list,            do: :gen_server.call(@server_name, :list)
  def step(pid),       do: Runner.continue(pid)
  def next(pid, expr), do: :gen_server.cast(@server_name, { :next, pid, expr })

  # those should be in UI
  def command(["list"], proc_list) do
    Enum.reduce proc_list, 0, fn({ pid, expr }, index) ->
      IO.puts "(#{index}) #{inspect pid}:\n\t#{Macro.to_string expr}"
      index + 1
    end
  end
  def command(["step", index], proc_list) do
    case Enum.at(proc_list, index) do
      { :ok, { pid, _ }} -> 
        step(pid)
      :error ->
        IO.puts "invalid index\n"
    end
  end
  def command(_, _), do: IO.puts "wat\n"
end
