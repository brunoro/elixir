defmodule IEx.Debugger.Evaluator do
  import IEx.Debugger.Escape

  def inspect_state(state, event, sep) do
    bin = ["\n#{inspect self} #{event} ", List.duplicate(sep, 10),
           "\n", inspect state.binding]
    bds = Enum.map Enum.with_index(state.stack), fn({{ binding, _ }, ind }) ->
      ["\n(", inspect(ind), ")\t", inspect(binding)]
    end
    IO.puts iolist_to_binary [bin | bds]
  end

  def eval_quoted(expr, state) do
    module    = elem(state.scope, 6)
    file      = elem(state.scope, 20)
    mod_scope = set_elem(state.scope, 14, module) # delegate_locals_to

    { _, meta, _ } = expr
    line = meta[:line] || 0

    try do
      # expand it: we just want to eval code
      ex_scope = :elixir_scope.to_ex_env({ line, mod_scope })
      exp = Macro.expand(expr, ex_scope)

      case Macro.safe_term(exp) do
        :ok ->
          { value, binding, scope } = { exp, state.binding, state.scope }
        { :unsafe, _ } ->
          { value, binding, scope } = :elixir.eval_quoted([expr], state.binding, line, mod_scope)
      end

      # some data is lost on scope conversion, such as module and file
      new_scope = scope 
                  |> update_binding(binding)
                  |> set_elem(6, module)
                  |> set_elem(14, module)
                  |> set_elem(20, file)

      { :ok, value, state.binding(binding).scope(new_scope) }
    catch
      kind, reason -> 
        { :exception, kind, reason, :erlang.get_stacktrace }
    end
  end

  def update_binding(scope, binding) do
    module = elem(scope, 6)
    { _mod, new_scope } = :elixir_scope.load_binding(binding, scope, module)
    new_scope
  end

  # add functions and pids to binding with some name mangling
  def escape_and_bind(thing, state) when is_escapable(thing) do 
    var     = thing |> escape |> binary_to_atom
    binding = Keyword.put(state.binding, var, thing)
    scope   = update_binding(state.scope, binding)

    new_state = state.binding(binding).scope(scope)
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

  # interface functions
  def expand(expr, state) do
    { _, meta, _ } = expr
    ex_scope = :elixir_scope.to_ex_env({ meta[:line] || 0, state.scope })
    { :ok, Macro.expand(expr, ex_scope) }
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
          { :after, [], unquote(after_esc) }
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
          { :after, [], unquote(after_esc) }
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  def initialize_clause_vars({ :->, _meta, clauses }, state) do
    match_clause = { [:__initialize_clause_vars__], [], :ok }
    all_clauses = { :->, [], [match_clause | clauses] }

    match_clause_case = quote do
      case :__initialize_clause_vars__ do
        unquote(all_clauses)
      end
    end

    escape_and_eval(match_clause_case, state)
  end

  def escape_clauses({ :->, meta, clauses }) do
   clause_list = Enum.map clauses, fn(clause) ->
      { left, _, _ } = clause
      esc_clause = Macro.escape clause

      { left, [], esc_clause }
    end

    { :->, meta, clause_list }
   end
end
