defrecord IEx.Debugger.State, [binding: nil, scope: nil, stack: []]

defmodule IEx.Debugger.Companion do
  use GenServer.Behaviour

  alias IEx.Debugger.Controller
  alias IEx.Debugger.State

  defrecord Data, [state: State[], breakpoints: [], active_breakpoints: [], shell_next: false, expr: nil]

  # public interface
  def start_link(binding, scope, breakpoints // [], shell_next // false) do
    state = State[binding: binding, scope: scope]
    data = Data[state: state, breakpoints: breakpoints, shell_next: shell_next]
    :gen_server.start_link(__MODULE__, data, [])
  end

  # gen_server methods
  def init(data) do
    state = data.state
    scope = :elixir_scope.vars_from_binding(state.scope, state.binding)
    new_state = state.scope(scope)
    { :ok, data.state(new_state) }
  end
    
  ## handle_call
  def handle_call(:get_state, _sender, data) do
    { :reply, data.state, data}
  end
  
  def handle_call(:breakpoints, _sender, data) do
    { :reply, data.breakpoints, data}
  end
  
  def handle_call(:shell_next, _sender, data) do
    { :reply, data.shell_next, data}
  end

  def handle_call(:expr, _sender, data) do
    { :reply, data.expr, data}
  end

  def handle_call(:active_breakpoints, _sender, data) do
    { :reply, data.active_breakpoints, data}
  end

  def handle_call({ :next, expr }, { pid, _ref }, data) do
    # breakpoints
    active = Enum.filter data.breakpoints, fn({ file, line }) ->
      { _, meta, _ } = expr
      env_file = elem(data.state.scope, 18) # see comment on IEx.Debugger.Evaluator
      (meta[:line] == line) and (env_file == file)
    end

    response = if Enum.empty?(active) do
      # breakpoints have priority over shell_next
      if (data.shell_next) do
        Controller.shell_next(false)
        Controller.start_shell(pid)
      end
      :go
    else
      Controller.start_shell(pid)
      :wait
    end

    { :reply, response, data.active_breakpoints(active).expr(expr) }
  end

  ## handle_cast
  def handle_cast(:done, data) do
    { :stop, :normal, data }
  end

  def handle_cast(:pop_stack, data) do
    new_state = case data.state.stack do
      [] ->
        data.state
      [{ binding, scope } | rest] ->
        State[binding: binding, scope: scope, stack: rest]
    end
    { :noreply, data.state(new_state) }
  end

  def handle_cast(:push_stack, data) do
    state = data.state
    new_state = state.stack([{ state.binding, state.scope } | state.stack])
    { :noreply, data.state(new_state) }
  end

  def handle_cast({ :put_state, new_state }, data) do
    { :noreply, data.state(new_state) }
  end
  
  def handle_cast({ :breakpoints, breakpoints}, data) do
    { :noreply, data.breakpoints(breakpoints) }
  end
  
  def handle_cast({ :shell_next, bool }, data) do
    { :noreply, data.shell_next(bool) }
  end

  # controller functions
  def expr(pid),               do: :gen_server.call(pid, :expr)
  def shell_next(pid),         do: :gen_server.call(pid, :shell_next)
  def shell_next(pid, bool),   do: :gen_server.cast(pid, { :shell_next, bool })
  def breakpoints(pid),        do: :gen_server.call(pid, :breakpoints)
  def breakpoints(pid, bp),    do: :gen_server.cast(pid, { :breakpoints, bp })
  def active_breakpoints(pid), do: :gen_server.call(pid, :active_breakpoints)

  # client functions
  def done(pid),             do: :gen_server.cast(pid, :done)
  def next(pid, expr),       do: :gen_server.call(pid, { :next, expr })
  def eval(pid, expr),       do: :gen_server.call(pid, { :eval, expr })

  def get_state(pid),        do: :gen_server.call(pid, :get_state)
  def put_state(pid, state), do: :gen_server.cast(pid, { :put_state, state })

  def pop_stack(pid),        do: :gen_server.cast(pid, :pop_stack)
  def push_stack(pid),       do: :gen_server.cast(pid, :push_stack)
end
