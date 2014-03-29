defrecord IEx.Debugger.State, [binding: nil, env: nil, stack: []]

defmodule IEx.Debugger.Companion do
  use GenServer.Behaviour
  alias IEx.Debugger.State

  defrecord Data, [state: State[],
                   breakpoints: [], active_breakpoints: [], 
                   pause_next: false, expr: nil]

  # public interface
  def start_link(binding, env, breakpoints \\ []) do
    state = State[binding: binding, env: env]
    data = Data[state: state, breakpoints: breakpoints]

    :gen_server.start_link(__MODULE__, data, [])
  end

  # gen_server methods
  def init(data) do
    { :ok, data }
  end
    
  ## handle_call
  def handle_call(:get_state, _sender, data) do
    { :reply, data.state, data}
  end
  
  def handle_call(:breakpoints, _sender, data) do
    { :reply, data.breakpoints, data}
  end
  
  def handle_call(:expr, _sender, data) do
    { :reply, data.expr, data}
  end

  def handle_call(:active_breakpoints, _sender, data) do
    { :reply, data.active_breakpoints, data}
  end

  def handle_call(:process_status, _sender, data) do
    { :elixir_env, _, file, line, _, _, _, _, _, _, _, _, _, _, _, _ } = data.state.env
    line = cond do
      not nil?(line) ->
        line
      { _, meta, _ } = data.expr ->
        meta[:line]
      true ->
        nil
    end

    # TODO: get expr from the file
    expr = data.expr

    # TODO: should we add a `running` field to Companion.Data?
    status = case data.active_breakpoints do
      []     -> :running
      _other -> :paused
    end
    { :reply, { status, file, line, expr }, data }
  end

  def handle_call({ :next, expr }, { pid, _ref }, data) do
    # breakpoints
    active_breakpoints = case expr do
      { _, meta, _ } ->
        { :elixir_env, _, file, line, _, _, _, _, _, _, _, _, _, _, _, _ } = data.state.env
        env_file = file
        env_line = line || meta[:line]
        Enum.filter data.breakpoints, fn({ bp_file, bp_line }) ->
          (env_line == bp_line) and (env_file == bp_file)
        end
      _other ->
        []
    end

    # breakpoints have priority over pause_next
    response = if Enum.empty?(active_breakpoints) do
      if (data.pause_next) do
        IO.puts "process #{inspect pid} paused"
        :wait
      else
        :go
      end
    else
      IO.puts "hit breakpoint at #{inspect pid}: #{inspect active_breakpoints}"
      :wait
    end

    data = data.pause_next(false).active_breakpoints(active_breakpoints).expr(expr)
    { :reply, response, data }
  end

  def handle_call(:pop_stack, from, data) do
    # If an empty stack is being popped, we can kill the Companion
    case data.state.stack do
      [] ->
        :gen_server.reply(from, :done)
        { :stop, :normal, data }

      stack ->
        [{ binding, env } | rest] = stack
        new_state = State[binding: binding, env: env, stack: rest]
        { :reply, :ok, data.state(new_state) }
    end
  end

  ## handle_cast
  def handle_cast({ :push_stack, binding, env}, data) do
    state = data.state
    new_state = state.binding(binding)
                     .env(env)
                     .stack([{ state.binding, state.env} | state.stack])

    { :noreply, data.state(new_state) }
  end

  def handle_cast({ :put_state, new_state }, data) do
    { :noreply, data.state(new_state) }
  end
  
  def handle_cast({ :breakpoints, breakpoints}, data) do
    { :noreply, data.breakpoints(breakpoints) }
  end
  
  def handle_cast(:pause_next, data) do
    { :noreply, data.pause_next(true) }
  end

  # controller functions
  def expr(pid),                       do: :gen_server.call(pid, :expr)
  def pause_next(pid),                 do: :gen_server.cast(pid, :pause_next)
  def breakpoints(pid),                do: :gen_server.call(pid, :breakpoints)
  def breakpoints(pid, bp),            do: :gen_server.cast(pid, { :breakpoints, bp })
  def active_breakpoints(pid),         do: :gen_server.call(pid, :active_breakpoints)
  def process_status(pid),             do: :gen_server.call(pid, :process_status)

  # client functions
  def next(pid, expr),                 do: :gen_server.call(pid, { :next, expr })

  def get_state(pid),                  do: :gen_server.call(pid, :get_state)
  def put_state(pid, state),           do: :gen_server.cast(pid, { :put_state, state })

  def pop_stack(pid),                  do: :gen_server.call(pid, :pop_stack)
  def push_stack(pid, binding, env), do: :gen_server.cast(pid, { :push_stack, binding, env})
end
