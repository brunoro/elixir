defmodule IEx.Debugger.Runner do
  alias IEx.Debugger.Companion
  alias IEx.Debugger.Evaluator
  alias IEx.Debugger.PIDTable
  alias IEx.Debugger.Runner

  # functions manipulating state coming from Companion
  defp change_state(comp, fun) do
    state = Companion.get_state(comp)

    case fun.(state) do
      { :exception, kind, reason, stacktrace } ->
        { :exception, kind, reason, stacktrace }
      { status, result, new_state } ->
        Companion.put_state(comp, new_state)
        { status, result }
    end
  end

  defp eval_change_state(comp, expr) do
    change_state comp, &Evaluator.escape_and_eval(expr, &1)
  end

  defp with_state(comp, fun) do
    state = Companion.get_state(comp)

    case fun.(state) do
      { :exception, kind, reason, stacktrace } ->
        { :exception, kind, reason, stacktrace }
      { status, result, _state } ->
        { status, result }
      { status, result } ->
        { status, result }
    end
  end

  defp eval_with_state(comp, expr) do
    with_state comp, &Evaluator.escape_and_eval(expr, &1)
  end

  defp kernel_macros(comp) do
    state = Companion.get_state(comp)
    elem(state.scope, 23)[Kernel] || [] 
  end

  # expand expr and run fun/0 
  defp do_or_expand(comp, expr, fun) do 
    { :ok, expanded } = with_state comp, &Evaluator.expand(expr, &1)

    { name, _, args } = expr 
    arity = if is_list(args), do: Enum.count(args), else: 0
        
    cond do
      # TODO: this is quite tricky
      { name, arity } in kernel_macros(comp) ->
        eval_change_state(comp, expr)
      # other macro, just eval it
      expanded != expr ->
        eval_change_state(comp, expr)
      # otherwise keep moving
      true ->
        fun.()
    end
  end

  # maps fun |> filter over col while fun |> condition is true
  # otherwise returns fun(failing_element)
  defp filter_map_while(col, condition, filter, fun) do 
    ret = do_filter_map_while(col, condition, filter, fun, [])
    case ret do
      list when is_list(list) ->
        Enum.reverse list
      other ->
        other
    end
  end

  defp do_filter_map_while([], _con, _fil, _fun, acc), do: acc
  defp do_filter_map_while([h | t], con, fil, fun, acc) do
    fh = fun.(h)
    if con.(fh) do
      do_filter_map_while(t, con, fil, fun, [fil.(fh) | acc])
    else
      fh
    end
  end

  # maps next/2 while status returned is :ok, otherwise returns the
  # failing element of the list with its status
  defp map_next_while_ok(comp, expr_list) do
    v = filter_map_while(expr_list, &is_status_ok?(&1), &strip_status(&1), &next(comp, &1))
    case v do
      value_list when is_list(value_list) ->
        { :ok, value_list }
      other ->
        other
    end
  end

  # result has the { status, result } form
  # runs fun(result) if status matches the parameter
  # otherwise returns result
  defp if_status(status, result, fun) do
    case result do
      { ^status, value } ->
        fun.(value)
      other ->
        other
    end
  end

  defp is_status_ok?({ status, _ }), do: status == :ok
  defp is_status_ok?({ status, _, _ }), do: status == :ok
  defp is_status_ok?({ status, _, _, _ }), do: status == :ok

  # removes status from a Runner return value
  def strip_status({ _, a }), do: a
  def strip_status({ _, a, b }), do: { a, b }

  # prepare values for injecting into quoted source
  defp prepare_value({ :fn, meta, clauses }), do: { :fn, meta, clauses }
  defp prepare_value(other),                  do: Macro.escape(other)

  # crawls on first quoted tree returning it with { :fn, _, _ } nodes
  # replaced by those on the second tree
  defp fn_from_second({ :fn, _, _ }, second), do: second
  defp fn_from_second(l1, l2) when is_list(l1) and is_list(l2) do
    Enum.map(Enum.zip(l1, l2), fn({ first, second }) ->
      fn_from_second(first, second)
    end)
  end
  defp fn_from_second({ left, meta, r1 }, { left, _, r2 }), do: { left, meta, fn_from_second(r1, r2) }
  defp fn_from_second(first, _),                            do: first

  defp receive_eval_or_go(comp) do
    receive do
      :go -> 
        :ok
      { :eval, from, expr } ->
        from <- { self, eval_change_state(comp, expr) }
        receive_eval_or_go(comp)
    end
  end

  defp authorize(comp, expr) do
    case Companion.next(comp, expr) do
      :wait -> 
        receive_eval_or_go(comp)
      :go -> 
        :ok
    end
  end

  ## next/2
  # makes nested next calls until leafs are reached.
  # keeps the current scope and binding
  # returns { :ok, value } or { :exception, kind, reason, stacktrace }
  def next(comp, expr) do
    do_next(comp, expr)
  end

  # anonymous functions
  def do_next(_comp, expr={ :fn, meta, [body] }) do
    next_body = wrap_next_arrow(body)
    { :ok, { :fn, meta, [next_body] }}
  end

  # case
  def do_next(comp, expr={ :case, meta, [condition | [[do: clauses]]] }) do
    { :ok, condition_value } = next(comp, condition)
    condition_value = prepare_value(condition_value)

    expr_condition_value = { :case, meta, [condition_value | [[do: clauses]]] }
    authorize(comp, fn_from_second(expr_condition_value, expr))
    match_next(comp, condition_value, clauses) # is there more than do?
  end

  # receive
  def do_next(comp, expr={ :receive, _, [[do: clauses]] }) do
    authorize(comp, expr)
    receive_next(comp, clauses) 
  end

  # receive-after
  def do_next(comp, expr={ :receive, _, [[do: nil, after: after_clause]] }) do
    authorize(comp, expr)
    {:->, _, [{ [after_time], _, after_expr }]} = after_clause

    receive_next(comp, after_time, after_expr) 
  end

  def do_next(comp, expr={ :receive, _, [[do: do_clauses, after: after_clause]] }) do
    authorize(comp, expr)
    {:->, _, [{ [after_time], _, after_expr }]} = after_clause

    receive_next(comp, do_clauses, after_time, after_expr) 
  end

  # try
  def do_next(comp, expr={ :try, _, [clauses] }) do
    authorize(comp, expr)
    do_clause = clauses[:do]
    # variables defined on try block aren't accessible outside it
    do_result = next(comp, do_clause)

    case do_result do
      { :exception, kind, reason, stacktrace } ->
        exception = { :exception, kind, reason, stacktrace }
        exception_next(comp, exception, clauses[:rescue], clauses[:catch])
      { :ok, value } ->
        if clauses[:else] do
          match_next(comp, value, clauses[:else])
        else
          do_result
        end
    end
  end

  # assignments
  def do_next(comp, expr={ :=, meta, [left | [right]] }) do
    if_status :ok, next(comp, right), fn(right_value) ->
      expr_value = { :=, meta, [left | [prepare_value(right_value)]] }
      authorize(comp, fn_from_second(expr_value, expr))
      eval_change_state(comp, expr_value)
    end
  end

  # list of expressions
  def do_next(comp, expr={ type, meta, expr_list }) when is_list(expr_list) do
    do_or_expand comp, expr, fn ->
      if_status :ok, map_next_while_ok(comp, expr_list), fn(value_list) ->
        nice_value_list = Enum.map(value_list, &prepare_value/1)
        expr_value = { type, meta, nice_value_list }
        authorize(comp, fn_from_second(expr_value, expr))
        eval_change_state(comp, expr_value)
      end
    end
  end

  # other triple expressions are evaluated directly
  def do_next(comp, expr={ _left, _meta, _right }) do
    do_or_expand comp, expr, fn ->
      eval_change_state(comp, expr)
    end
  end

  # list building operator comes wrapped in lists
  def do_next(comp, [{ :|, _meta, expr_list }]) when is_list(expr_list) do
    case map_next_while_ok(comp, expr_list) do
      { :exception, kind, reason, stacktrace } ->
        { :exception, kind, reason, stacktrace }
      { :ok, [head, tail]} ->
        { :ok, [head | tail] }
    end
  end

  # lists aren't escaped like tuples
  def do_next(comp, expr_list) when is_list(expr_list) do
    authorize(comp, expr_list)
    map_next_while_ok(comp, expr_list)
  end
  
  # other tuples?
  def do_next(comp, expr_tuple) when is_tuple(expr_tuple) do
    authorize(comp, expr_tuple)

    expr_list = tuple_to_list(expr_tuple)
    case map_next_while_ok(comp, expr_list) do
      { :exception, kind, reason, stacktrace } ->
        { :exception, kind, reason, stacktrace }
      { :ok, result } ->
        { :ok, list_to_tuple(result) }
    end
  end

  # ignore everything else (atoms, binaries, numbers, etc.)
  def do_next(_, other), do: { :ok, other }

  # pattern matching operator should evaluate clauses until
  # the first clause matching the condition is found
  def match_next(comp, value, clauses) do
    matching_clause = change_state comp, fn(state) ->
      Evaluator.find_match_clause(prepare_value(value), clauses, state)
    end
    
    if_status :ok, matching_clause, fn({ _, _, right }) ->
      result = next(comp, right)

      change_state comp, fn(state) ->
        Evaluator.initialize_clause_vars(clauses, state)
      end

      result
    end
  end

  # receive block
  def receive_next(comp, clauses) do
    matching_clause = change_state comp, fn(state) ->
      Evaluator.find_receive_clause(clauses, state)
    end
    
    if_status :ok, matching_clause, fn({ _, _, right }) ->
      result = next(comp, right)

      change_state comp, fn(state) ->
        Evaluator.initialize_clause_vars(clauses, state)
      end

      result
    end
  end

  # receive / after block with no receive clauses
  def receive_next(comp, after_time, after_clause) do
    matching_clause = change_state comp, fn(state) ->
      Evaluator.find_receive_clause(after_time, after_clause, state)
    end
    
    if_status :ok, matching_clause, fn({ _, _, right }) ->
      next(comp, right)
    end
  end

  # receive / after block
  def receive_next(comp, clauses, after_time, after_clause) do
    matching_clause = change_state comp, fn(state) ->
      Evaluator.find_receive_clause(clauses, after_time, after_clause, state)
    end
    
    if_status :ok, matching_clause, fn({ _, _, right }) ->
      result = next(comp, right)

      change_state comp, fn(state) ->
        Evaluator.initialize_clause_vars(clauses, state)
      end

      result
    end
  end

  def exception_next(comp, exception, rescue_block, catch_block) do
    { :exception, kind, reason, stacktrace } = exception
    esc_stacktrace = Macro.escape stacktrace
    esc_reason = Macro.escape reason

    clauses = [do: quote do
      :erlang.raise(unquote(kind), unquote(esc_reason), unquote(esc_stacktrace))
    end]

    if rescue_block, do: clauses = 
      Keyword.put clauses, :rescue, wrap_next_arrow(rescue_block)
    if catch_block, do: clauses = 
      Keyword.put clauses, :catch, wrap_next_arrow(catch_block)

    try_expr = { :try, [context: IEx.Debugger.Evaluator, import: Kernel], [clauses] }

    if try_expr do
      eval_with_state(comp, try_expr)
    else
      exception
    end
  end

  def wrap_next_arrow({ :->, meta, clauses }) do
    wrap_clauses = Enum.map clauses, fn({ left, clause_meta, right }) ->
      { left, clause_meta, wrap_next_clause(right) }
    end
    { :->, meta, wrap_clauses }
  end

  # wrap_next_clause/1 prepares an expression to call next/1
  # inside an :elixir.eval call. This is used on case clauses and
  # anonymous functions
  def wrap_next_clause(expr) do
    esc_expr = Macro.escape expr

    quote do
      case PIDTable.get do
        nil ->
          binding = Kernel.binding
          scope   = __ENV__ 
                    |> :elixir_scope.to_erl_env
                    |> Evaluator.update_binding(binding)
        companion ->
          state   = Companion.get_state(companion)
          binding = Keyword.merge(state.binding, Kernel.binding)
          scope   = Evaluator.update_binding(state.scope, binding)
      end
      
      module = __ENV__.module
      scope = scope
              |> set_elem(20, __FILE__)
              |> set_elem(6, module)
              |> set_elem(14, module)

      comp = PIDTable.start(binding, scope)
      return = Runner.next(comp, unquote(esc_expr))
      PIDTable.finish

      case return do
          { :ok, value } ->
            value
          { :exception, kind, reason, stacktrace } ->
            :erlang.raise(kind, reason, stacktrace)
      end
    end
  end
end
