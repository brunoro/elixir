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
    { :reply, dict, dict }
  end

  def handle_call({ :get, pid }, _sender, dict) do
    case dict[pid] do
      nil ->
        { :reply, nil, dict }
      companion ->
        { :reply, companion, dict }
    end
  end
 
  def handle_call(:get, { pid, _ref }, dict) do
    case dict[pid] do
      nil ->
        { :reply, nil, dict }
      companion ->
        { :reply, companion, dict }
    end
  end
  
  # before function calls
  def handle_call({ :start, binding, env }, { pid, _ref }, dict) do
    { dict, companion } = case dict[pid] do
      nil ->
        # fetch info from any other Companion or from the Controller
        bps = case Enum.at(dict, 0) do
          nil ->
            Controller.breakpoints
          { _pid, comp } ->
            Companion.breakpoints(comp)
        end

        { :ok, companion } = Companion.start_link(binding, env, bps)
        { Dict.put(dict, pid, companion), companion }

      companion ->
        Companion.push_stack(companion, binding, env)
        { dict, companion }
    end

    { :reply, companion, dict }
  end
 
  # after function calls
  def handle_call(:finish, { pid, _ref }, dict) do
    new_dict = case Companion.pop_stack(dict[pid]) do
      :ok   -> dict
      :done -> Dict.delete(dict, pid)
    end

    { :reply, :ok, new_dict }
  end

  # controller interface
  def get_all,               do: :gen_server.call(@server_name, :get_all)
  def get(pid),              do: :gen_server.call(@server_name, { :get, pid })

  # client interface
  def get,                 do: :gen_server.call(@server_name, :get)
  def start(binding, env), do: :gen_server.call(@server_name, { :start, binding, env })
  def finish,              do: :gen_server.call(@server_name, :finish)

end
