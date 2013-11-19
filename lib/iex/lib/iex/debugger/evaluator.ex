defmodule IEx.Debugger.Evaluator do
  import IEx.Debugger.Escape

  # Elixir scope -- Erlang record: 
  # 0   :elixir_scope,
  # 1   context=nil,             %% can be assign, guards or nil
  # 2   extra=nil,               %% extra information about the context, like fn_match for fns
  # 3   noname=false,            %% when true, don't add new names (used by try)
  # 4   super=false,             %% when true, it means super was invoked
  # 5   caller=false,            %% when true, it means caller was invoked
  # 6   module=nil,              %% the current module
  # 7   function=nil,            %% the current function
  # 8   vars=[],                 %% a dict of defined variables and their alias
  # 9   list_vars=nil,           %% a list of vars passed down to Macro.Env
  # 10  backup_vars=nil,         %% a copy of vars to be used on ^var
  # 11  temp_vars=nil,           %% a set of all variables defined in a particular assign
  # 12  clause_vars=nil,         %% a dict of all variables defined in a particular clause
  # 13  extra_guards=nil,        %% extra guards from args expansion
  # 14  counter=[],              %% a counter for the variables defined
  # 15  local=nil,               %% the scope to evaluate local functions against
  # 16  context_modules=[],      %% modules defined in the current context
  # 17  macro_aliases=[],        %% keep aliases defined inside a macro
  # 18  aliases,                 %% an orddict with aliases by new -> old names
  # 19  file,                    %% the current scope filename
  # 20  requires,                %% a set with modules required
  # 21  macro_macros=[],         %% a list with macros imported from module inside a macro
  # 22  macros,                  %% a list with macros imported from module
  # 23  macro_functions=[],      %% a list with functions imported from module inside a macro
  # 24  functions                %% a list with functions imported from module
  def eval_quoted(expr, state) do
    module    = elem(state.scope, 6)
    file      = elem(state.scope, 19)
    mod_scope = set_elem(state.scope, 15, module) # delegate_locals_to

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
                  |> set_elem(15, module) # local
                  |> set_elem(19, file)

      { :ok, value, state.binding(binding).scope(new_scope) }
    catch
      kind, reason -> 
        { :exception, kind, reason, :erlang.get_stacktrace }
    end
  end

  # add functions and pids to binding with some name mangling
  def escape_and_bind(thing, state) when is_escapable(thing) do 
    var = thing |> escape |> binary_to_atom
    new_binding = Keyword.put(state.binding, var, thing)
    new_scope = :elixir_scope.vars_from_binding(state.scope, new_binding)

    {{ var, [], nil }, state.binding(new_binding).scope(new_scope) }
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
    { :ok, Macro.expand_once(expr, ex_scope) }
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
