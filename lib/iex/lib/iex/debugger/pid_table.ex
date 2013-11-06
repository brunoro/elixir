defmodule IEx.Debugger.PIDTable do
  use GenServer.Behaviour

  alias IEx.Debugger.Controller
  alias IEx.Debugger.Companion

  @server_name { :global, :pid_table }

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

  def init(_opts) do
    { :ok, HashDict.new }
  end

  def handle_call( :stop, _sender, dict ) do
    { :stop, :normal, :shutdown_ok, dict }
  end

  def handle_call(:get_all, _sender, dict) do
    IO.inspect dict
    { :reply, dict, dict }
  end

  def handle_call({ :get, pid }, _sender, dict) do
    case dict[pid] do
      { companion, _count } ->
        { :reply, companion, dict }
      nil ->
        { :reply, nil, dict }
    end
  end
  
  # before function calls
  def handle_call({ :start, pid, binding, scope }, _sender, dict) do
    entry = case dict[pid] do
      { companion, count } ->
        # create new context
        Companion.push_stack(companion)
        state = Companion.get_state(companion)
        Companion.put_state(companion, state.binding(binding).scope(scope))

        { companion, count + 1 }
      nil ->
        # fetch info from any other Companion or from the Controller
        { bps, shn } = case Enum.first(dict) do
          nil ->
            { Controller.breakpoints, Controller.shell_next }
          { _pid, { comp, _ }} ->
            { Companion.breakpoints(comp), Companion.shell_next(comp) }
        end

        { :ok, companion } = Companion.start_link(binding, scope, bps, shn)
        { companion, 0 }
    end

    { :reply, companion, Dict.put(dict, pid, entry) }
  end
 
  # after function calls
  def handle_cast({ :finish, pid }, dict) do
    new_dict = case dict[pid] do
      { companion, 0 } ->
        Companion.done(companion)
        Dict.delete(dict, pid)
      { companion, count } ->
        # exit context
        Companion.pop_stack(companion)
        Dict.put(dict, pid, { companion, count - 1 })
    end

    { :noreply, new_dict }
  end

  # controller interface
  def get_all,                    do: :gen_server.call(@server_name, :get_all)

  # client interface
  def get(pid),                   do: :gen_server.call(@server_name, { :get, pid })
  def start(pid, binding, scope), do: :gen_server.call(@server_name, { :start, pid, binding, scope })
  def finish(pid),                do: :gen_server.cast(@server_name, { :finish, pid })

end
