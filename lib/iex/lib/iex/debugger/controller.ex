defmodule IEx.Debugger.Controller do
  use GenServer.Behaviour

  alias IEx.Debugger.Companion
  alias IEx.Debugger.Runner
  alias IEx.Debugger.PIDTable
  alias IEx.Debugger.Shell

  @server_name { :global, :controller }

  defrecord Data, [breakpoints: [], shell_next: []]

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
    { :ok, Data[] }
  end

  def handle_cast({ :shell_next, shell_next }, data) do
    companions = Enum.map(Dict.values(PIDTable.get_all), fn({ pid, _count}) -> pid end)
    Enum.each(companions, &(Companion.shell_next(&1, shell_next)))
    { :noreply, data.shell_next(shell_next) }
  end

  def handle_cast({ :breakpoints, breakpoints }, data) do
    companions = Enum.map(Dict.values(PIDTable.get_all), fn({ pid, _count}) -> pid end)
    Enum.each(companions, &(Companion.breakpoints(&1, breakpoints)))
    { :noreply, data.breakpoints(breakpoints) }
  end

  def handle_call(:stop, _sender, data) do
    { :stop, :normal, :shutdown_ok, data }
  end
  
  def handle_call({ :eval, pid, expr }, _sender, data) do
    # TODO: check if pid is on a breakpoint
    pid <- { :eval, self, expr }
    receive do
      { ^pid, result } ->
        { :reply, result, data }
    end
  end
  
  def handle_call(:list, _sender, data) do
    pid_status = Enum.map PIDTable.get_all, fn({ pid, { companion, _count }}) ->
      status = case Companion.active_breakpoints(companion) do
        [] -> "running (#{inspect Companion.expr(companion)}"
        other -> inspect(Enum.first(other)) # { file, line } string
      end
      { pid, status }
    end

    { :reply, pid_status, data }
  end
  
  def handle_call({ :binding, pid }, _sender, data) do
    companion = PIDTable.get(pid)
    binding = if companion do Companion.get_state(companion).binding else nil end
    { :reply, binding, data }
  end

  def handle_call(:breakpoints, _sender, data) do
    { :reply, data.breakpoints, data }
  end

  def handle_call(:shell_next, _sender, data) do
    { :reply, data.shell_next, data }
  end
  
  # interface methods
  def eval(pid, expr),  do: :gen_server.call(@server_name, { :eval, pid, expr })
  def shell_next,       do: :gen_server.call(@server_name, :shell_next)
  def shell_next(bool), do: :gen_server.cast(@server_name, { :shell_next, bool })
  def breakpoints,      do: :gen_server.call(@server_name, :breakpoints)
  def breakpoints(pat), do: :gen_server.cast(@server_name, { :breakpoints, pat })
  def list,             do: :gen_server.call(@server_name, :list)
  def binding(pid),     do: :gen_server.call(@server_name, { :binding, pid })

  # companion methods
  def start_shell(pid) do
    timeout = 1000
    message = "Debug event at #{inspect pid}, starting debug shell"
    opts = [dot_iex_path: "", prefix: "dbg"]
    spawn fn -> Shell.take_over(message, opts, timeout) end
  end
end
