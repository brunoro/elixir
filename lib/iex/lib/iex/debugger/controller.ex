defmodule IEx.Debugger.Controller do
  use GenServer.Behaviour

  alias IEx.Debugger.Companion
  alias IEx.Debugger.PIDTable

  @server_name { :global, :controller }

  defrecord Data, [breakpoints: [], modules: []]

  def alive? do
    { _reg, name } = @server_name
    :global.whereis_name(name) != :undefined
  end

  def start_link(opts \\ []) do
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

  ## handle_cast
  def handle_cast({ :breakpoints, breakpoints }, data) do
    companions = Enum.map(Dict.values(PIDTable.get_all), fn(pid) -> pid end)
    Enum.each(companions, &(Companion.breakpoints(&1, breakpoints)))
    { :noreply, data.breakpoints(breakpoints) }
  end

  def handle_cast({ :modules, modules }, data) do
    { :noreply, data.modules(modules) }
  end

  ## handle_call
  def handle_call({ :binding, pid }, _sender, data) do
    companion = PIDTable.get(pid)
    binding = if companion do Companion.get_state(companion).binding else nil end
    { :reply, binding, data }
  end

  def handle_call(:breakpoints, _sender, data) do
    { :reply, data.breakpoints, data }
  end

  def handle_call({ :eval, pid, expr }, _sender, data) do
    if_at_breakpoint pid, fn ->
      send pid, { :eval, self, expr }
      receive do
        { ^pid, result } ->
          { :reply, result, data }
      end
    end
  end
  
  def handle_call(:list, _sender, data) do
    pid_status = Enum.map PIDTable.get_all, fn({ pid, companion }) ->
      status = Companion.process_status(companion)
      { pid, status }
    end

    { :reply, pid_status, data }
  end
  
  def handle_call(:modules, _sender, data) do
    { :reply, data.modules, data }
  end
  
  def handle_call({ :pause_next, pid }, _sender, data) do
    reply = cond do
      not Process.alive?(pid) ->
        :noproc
      at_breakpoint?(pid) ->
        :ok
      true ->
        companion = PIDTable.get(pid)
        if companion do Companion.pause_next(companion) else :noproc end
    end

    { :reply, reply, data }
  end

  def handle_call(:stop, _sender, data) do
    { :stop, :normal, :shutdown_ok, data }
  end
  
  def handle_call({ :run, pid }, _sender, data) do
    if_at_breakpoint pid, fn ->
      send pid, :go
    end

    { :noreply, data }
  end
 
  # interface methods
  def binding(pid),     do: :gen_server.call(@server_name, { :binding, pid })
  def breakpoints,      do: :gen_server.call(@server_name, :breakpoints)
  def breakpoints(pat), do: :gen_server.cast(@server_name, { :breakpoints, pat })
  def eval(pid, expr),  do: :gen_server.call(@server_name, { :eval, pid, expr })
  def list,             do: :gen_server.call(@server_name, :list)
  def modules,          do: :gen_server.call(@server_name, :modules)
  def modules(mod),     do: :gen_server.cast(@server_name, { :modules, mod })
  def pause_next(pid),  do: :gen_server.call(@server_name, { :pause_next, pid })
  def run(pid),         do: :gen_server.call(@server_name, { :run, pid })

  # fun/0 if pid is at a breakpoint, otherwise :running or :norproc
  defp if_at_breakpoint(pid, fun) do
    cond do
      not Process.alive?(pid) ->
        :noproc
      at_breakpoint?(pid) ->
        fun.()
      true ->
        :running
    end
  end

  defp at_breakpoint?(pid) do
    companion = PIDTable.get(pid)
    not Enum.empty?(Companion.active_breakpoints(companion))
  end
end
