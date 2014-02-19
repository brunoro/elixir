defmodule IEx.Debugger.Evaluator do
  import IEx.Debugger.Escape

  def inspect_state(state, event, sep) do
    bin = [sep, sep, inspect(self), 
           " ", sep, sep, " ", event, " ",
           List.duplicate(sep, 10),
           "\n", inspect(state.binding)]
    bds = Enum.map Enum.with_index(state.stack), fn({{ binding, _ }, ind }) ->
      ["\n(", inspect(ind), ")\t", inspect(binding)]
    end
    IO.puts iolist_to_binary [bin, bds, "\n", List.duplicate(sep, 40), "\n"]
  end

  def eval_quoted(expr, state) do
    try do
      # expand it: we just want to eval code
      { :ok, exp } = expand(expr, state)

      { value, binding, env, _scope } = case Macro.safe_term(exp) do
        { :unsafe, { var, _, nil }} when is_atom(var) ->
          case state.binding[var] do
            nil ->
              :elixir.eval_quoted(expr, state.binding, state.env)
            value ->
              { value, state.binding, state.env, nil }
          end
        _other ->
          :elixir.eval_quoted(expr, state.binding, state.env)
      end

      { :ok, value, state.binding(binding).env(env) }
    catch
      kind, reason -> 
        { :exception, kind, reason, :erlang.get_stacktrace }
    end
  end

  def update_binding(env, binding) do
    # TODO: why vars are stored as { :var, nil }?
    # { :elixir_env, _, _, _, _, _, _, _, _, _, _, _, vars, _, _, _ } = env
    set_elem(env, 12, binding)
  end

  # add functions and pids to binding with some name mangling
  def escape_and_bind(thing, state) when is_escapable(thing) do 
    var     = thing |> escape |> binary_to_atom
    binding = Keyword.put(state.binding, var, thing)
    env     = update_binding(state.env, binding)

    new_state = state.binding(binding).env(env)
    {{ var, [], nil }, new_state }
  end
  # star trek: deep escape 9
  def escape_and_bind(list, state) when is_list(list) do
    { l, s } = Enum.reduce list, { [], state }, fn(expr, { acc, old_state }) ->
      { esc, new_state } = escape_and_bind(expr, old_state)
      { [esc | acc], new_state }
    end
    { Enum.reverse(l), s }
  end
  def escape_and_bind(tuple, state) when is_tuple(tuple) do
    list = tuple_to_list(tuple)
    { esc_list, new_state } = escape_and_bind(list, state)
    esc_tuple = list_to_tuple(esc_list)
    { esc_tuple, new_state }
  end
  def escape_and_bind(value, state), do: { value, state }

  def escape_and_eval(expr, state) do
    { esc_value, esc_state } = escape_and_bind(expr, state)
    eval_quoted(esc_value, esc_state)
  end

  def expr_line(expr) do
    { _, meta, _ } = expr
    meta[:line] || 0
  end

  # interface functions
  def expand(expr, state) do
    ex = :elixir_env.env_to_ex({ expr_line(expr), state.env })
    { :ok, Macro.expand(expr, ex) }
  end

  # generates `unquote(lhs) -> unquote(Macro.escape clause)`
  def find_match_clause(value, clauses, state) do 
    clause_list = escape_clauses(clauses)

    match_clause_case = quote do
      case unquote(value) do
        unquote(clause_list)
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  def find_receive_clause(clauses, state) do 
    clause_list = escape_clauses(clauses)

    match_clause_case = quote do
      receive do
        unquote(clause_list)
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  def find_receive_clause(after_time, after_clause, state) do 
    after_esc = Macro.escape(after_clause)

    match_clause_case = quote do
      receive do
      after
        unquote(after_time) ->
          [:__after__, unquote(after_esc)]
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  def find_receive_clause(clauses, after_time, after_clause, state) do 
    clause_list = escape_clauses(clauses)
    after_esc = Macro.escape(after_clause)

    match_clause_case = quote do
      receive do
        unquote(clause_list)
      after
        unquote(after_time) ->
          [:__after__, unquote(after_esc)]
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  # TODO: test this, as it seems the quoted expression for arrows changed
  def initialize_clause_vars(clauses, state) do
    match_clause = { :->, [], [[:__initialize_clause_vars__], :ok] }
    all_clauses = [match_clause | clauses]

    match_clause_case = quote do
      case :__initialize_clause_vars__ do
        unquote(all_clauses)
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  def escape_clauses(clauses) when is_list(clauses), do: Enum.map(clauses, &escape_clause/1)

  defp escape_clause({ :->, meta, clause=[left, _] }) do
    esc_clause = Macro.escape(clause)
    { :->, meta, [left, esc_clause] }
   end
end
