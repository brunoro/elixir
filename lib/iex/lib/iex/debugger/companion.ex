defmodule IEx.Debugger.Companion do
  use GenServer.Behaviour

  alias IEx.Debugger.Controller

  defrecord State, [binding: nil, scope: nil, stack: []]

  # public interface
  def start_link(binding, scope, breakpoints) do
    state = State[binding: binding, scope: scope]
    :gen_server.start_link(__MODULE__, { state, []}, [])
  end

  # gen_server methods
  def init({ state, breakpoints }) do
    scope = :elixir_scope.vars_from_binding(state.scope, state.binding)
    { :ok, { state.scope(scope), breakpoints }}
  end
    
  ## handle_call
  def handle_call(:get_state, _sender, { state, breakpoints }) do
    { :reply, state, { state, breakpoints }}
  end

  def handle_call({ :next, expr }, { pid, _ref }, { state, breakpoints }) do
    #state.client <- { :debug, { :next, pid, expr }}
    matching = Enum.filter breakpoints, fn({ file, line }) ->
      # TODO: this won't work for most expressions!!
      { _, meta, _ } = expr
      env_file = elem(state.scope, 19) # see comment on IEx.Debugger.Evaluator
      (meta[:line] == line) and (env_file == file)
    end
    unless Enum.empty? matching do
      Controller.breakpoint(matching, pid, expr)
    end
    { :reply, :ok, { state, breakpoints }}
  end

  def handle_call({ :eval, expr }, _sender, { state, breakpoints }) do
    case Evaluator.eval_quoted(expr, state) do
      { :ok, value, new_state } ->
        { :reply, { :ok, value }, { new_state, breakpoints }}
      exception ->
        {{ :reply, exception }, { state, breakpoints }}
    end
  end

  ## handle_cast
  def handle_cast(:done, data) do
    { :stop, :normal, data }
  end

  def handle_cast(:pop_stack, { state, breakpoints }) do
    new_state = case state.stack do
      [] ->
        state
      [{ binding, scope } | rest] ->
        State[binding: binding, scope: scope, stack: rest]
    end
    { :noreply, { new_state, breakpoints }}
  end

  def handle_cast(:push_stack, { state, breakpoints }) do
    new_state = state.stack([{ state.binding, state.scope } | state.stack])
    { :noreply, { new_state, breakpoints }}
  end

  def handle_cast({ :put_state, new_state }, { _state, breakpoints }) do
    { :noreply, { new_state, breakpoints }}
  end

  # controller functions
  def breakpoints(pid),       do: :gen_server.call(pid, :breakpoints)
  def breakpoints(pid, bp),   do: :gen_server.call(pid, { :breakpoints, bp })

  # client functions
  def done(pid),             do: :gen_server.cast(pid, :done)
  def next(pid, expr),       do: :gen_server.call(pid, { :next, expr })
  def eval(pid, expr),       do: :gen_server.call(pid, { :eval, expr })

  def get_state(pid),        do: :gen_server.call(pid, :get_state)
  def put_state(pid, state), do: :gen_server.cast(pid, { :put_state, state })

  def pop_stack(pid),        do: :gen_server.cast(pid, :pop_stack)
  def push_stack(pid),       do: :gen_server.cast(pid, :push_stack)
end
