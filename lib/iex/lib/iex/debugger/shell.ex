defmodule IEx.Debugger.Shell do
  @moduledoc false
  @break_trigger '#iex:break\n'

  alias IEx.Server
  alias IEx.Evaluator
  alias IEx.Debugger.Controller
  alias IEx.Debugger.PIDTable
  alias IEx.Debugger.Runner

  @doc """
  Requests to take over the given shell from the
  current process.
  """
  @spec take_over(binary, Keyword.t, pos_integer) ::
        :ok | { :error, :self } | { :error, :no_iex } | { :error, :refused }
  def take_over(identifier, opts, timeout \\ 1000, server \\ Server.whereis()) do
    cond do
      nil?(server) ->
        { :error, :no_iex }
      true ->
        ref = make_ref()
        send server, { :take?, self, ref }

        receive do
          ^ref ->
            opts = Keyword.put(opts, :evaluator, self)
            send server, { :take, self, identifier, ref, opts }

            receive do
              { ^ref, nil } ->
                { :error, :refused }
              { ^ref, leader } ->
                IEx.Debugger.Shell.start(server, leader)
                # TODO: fall back to the old evaluator after shell dies
            end
        after
          timeout ->
            { :error, :no_iex }
        end
        :ok
    end
  end

  def start(server, leader) do
    IEx.Debugger.start([])

    IEx.History.init
    old_leader = Process.group_leader
    old_flag   = Process.flag(:trap_exit, true)
    Process.group_leader(self, leader)

    try do
      loop(server)
    after
      Process.group_leader(self, old_leader)
      Process.flag(:trap_exit, old_flag)
    end
  end

  defp compile_do(latest_input, config, fun) do
    code = config.cache ++ String.to_char_list!(latest_input)
    case Code.string_to_quoted(code, [line: config.counter, file: "dbg"]) do
      { :ok, forms } ->
        fun.(forms)
      { :error, { line, error, token } } ->
        if token == [] do
          config.cache(code)
        else
          try do
            :elixir_errors.parse_error(line, "dbg", error, token)
          catch
            kind, error ->
              Evaluator.print_error(kind, error, System.stacktrace)
          end
          config
        end
    end
  end

  def pid_to_string(pid) do
    iolist_to_binary(:erlang.pid_to_list(pid))
  end

  @helpers_module IEx.Debugger.Helpers

  defp dbg_env do
    env = :elixir.env_for_eval(file: "dbg", delegate_locals_to: @helpers_module)
    require_helpers = String.to_char_list!("require #{to_string @helpers_module}")
    { _, _, new_env, _ } = :elixir.eval(require_helpers, [], env)
    new_env
  end

  # TODO: helpers may also be called using the module prefix
  defp helper_call?({ fun, _, args }) when is_atom(fun) and is_list(args) do
    function_exported?(@helpers_module, fun, Enum.count(args))
  end
  defp helper_call?({ fun, _, nil }) when is_atom(fun) do
    function_exported?(@helpers_module, fun, 0)
  end
  defp helper_call?(_), do: false

  defp loop(server) do
    receive do
      { :eval, ^server, "##\n", _config } ->
        :ok

      { :eval, ^server, code, config } ->
        config = compile_do code, config, fn(forms) -> 
          case forms do
            # TODO: ds helper needs the server pid as a parameter,
            #       there should be a better way to do this.
            { :ds, _, pid_expr } ->
                { pid, _, _, _ } = :elixir.eval_forms(pid_expr, config.binding, dbg_env)
                result = IEx.Debugger.Helpers.ds(pid, server, config)
                IO.puts :stdio, IEx.color(:eval_result, inspect result)

                config
            _other ->
              # Helpers are evaluated serially, but env isn't changed
              if helper_call?(forms) do
                config = Evaluator.eval(code, config.env(dbg_env))

              # Every other expression should be evaluated on a separate process
              else
                line = config.counter
                pid = spawn fn ->
                  companion = PIDTable.start(config.binding, dbg_env)

                  case Runner.next(companion, forms) do
                    { :ok, result } ->
                      str = "(#{inspect line})#{pid_to_string self} => #{inspect result}"
                      IO.puts :stdio, IEx.color(:eval_result, str)
                    { :exception, kind, reason, stacktrace } ->
                      Evaluator.print_error(kind, reason, stacktrace)
                  end
                  PIDTable.finish
                end

                IO.puts :stdio, IEx.color(:eval_info, pid_to_string(pid))

                Evaluator.update_history(line, code, pid)
                config.update_counter(&(&1+1)).cache('')
              end
          end
        end

        send server, { :evaled, self, config }
        loop(server)
        
      { :done, ^server } ->
        :ok

      { :EXIT, _other, :normal } ->
        loop(server)
      { :EXIT, other, reason } ->
        Server.print_exit(other, reason)
        loop(server)
    end
  end

  def process_shell_loop(server, pid) do
    receive do
      { :eval, ^server, "##\n", _config } ->
        :ok

      { :eval, ^server, code, config } ->
        config = compile_do code, config, fn(forms) -> 
          case Controller.eval(pid, forms) do
            { :ok, result } ->
              str = "(#{inspect config.counter})#{pid_to_string self} => #{inspect result}"
              IO.puts :stdio, IEx.color(:eval_result, str)
              Evaluator.update_history(config.counter, code, result)

            { :exception, kind, reason, stacktrace } ->
              Evaluator.print_error(kind, reason, stacktrace)
          end

          config.update_counter(&(&1+1)).cache('')
        end

        send server, { :evaled, self, config }
        process_shell_loop(server, pid)

      { :done, _server } ->
        :ok
      { :EXIT, _other, :normal } ->
        process_shell_loop(server, pid)
      { :EXIT, other, reason } ->
        Server.print_exit(other, reason)
        process_shell_loop(server, pid)
    end
  end
end
