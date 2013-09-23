defmodule IEx.Debugger.Controller do
  use GenServer.Behaviour

  alias IEx.Debugger.Runner
  alias IEx.Debugger.PIDTable
  alias IEx.Debugger.Shell

  @server_name { :global, :controller }

  defrecord State, [current_shell: nil, breakpoints: []]

  def alive? do
    { _reg, name } = @server_name
    :global.whereis_name(name) != :undefined
  end

  def start_link(opts // []) do
    { status, value } = :gen_server.start_link(@server_name, __MODULE__, opts, [])
    case status do
      :ok -> 
        pid = value
      :error -> 
        { :already_started, pid } = value
    end
    { :ok, pid }
  end

  def init([]) do
    { :ok, State[] }
  end

  def handle_call(:stop, _sender, dict) do
    { :stop, :normal, :shutdown_ok, dict }
  end
  
  def handle_call(:list, _sender, state) do
    processes = Dict.keys(PIDTable.get_all)
    { :reply, processes, state }
  end
  
  def handle_call(:breakpoints, _sender, state) do
    { :reply, state.breakpoints, state }
  end
  
  def handle_call({ :breakpoints, breakpoints }, _sender, state) do
    companions = Dict.values(PIDTable.get_all)
    Enum.each(companions, &(Companion.breakpoints(&1, breakpoints)))
    { :reply, breakpoints, state.breakpoints(breakpoints) }
  end

  # interface methods
  def breakpoints,        do: :gen_server.call(@server_name, :breakpoints)
  def breakpoints(pat),   do: :gen_server.call(@server_name, { :breakpoints, pat })
  def list,               do: :gen_server.call(@server_name, :list)

  def breakpoint(_matching, pid, expr) do
    timeout = 1000
    message = "Breakpoint at #{inspect pid}"
    opts = [dot_iex_path: "", prefix: "dbg"]
    Shell.take_over(message, opts, timeout)
    :ok
  end

  # TODO: those should be in some UI module
  def command(["list"], proc_list) do
    Enum.reduce proc_list, 0, fn({ pid, expr }, index) ->
      IO.puts "(#{index}) #{inspect pid}:\n\t#{Macro.to_string expr}"
      index + 1
    end
  end
  def command(_, _), do: IO.puts "wat\n"
end
