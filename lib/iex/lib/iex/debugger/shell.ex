defmodule IEx.Debugger.Shell do
  @moduledoc false
  @break_trigger '#iex:break\n'

  alias IEx.Config
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
  def take_over(identifier, opts, timeout // 1000, server // Server.whereis()) do
    cond do
      nil?(server) ->
        { :error, :no_iex }
      self == Server.whereis_evaluator(server) ->
        { :error, :self }
      true ->
        ref = make_ref()
        server <- { :take?, self, ref }

        receive do
          ^ref ->
            opts = Keyword.put(opts, :evaluator, self)
            server <- { :take, self, identifier, ref, opts }

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

  defp compile_do(code, config, fun) do
    list_code = String.to_char_list!(code)
    case :elixir_translator.forms(list_code, 0, "dbg", []) do
      { :ok, [forms] } ->
        config = fun.(forms)
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
        end
    end
  end

  defp helpers_scope(scope) do
    scope = :elixir.scope_for_eval(scope, delegate_locals_to: IEx.Debugger.Helpers)
    { _, _, scope } = :elixir.eval('require IEx.Debugger.Helpers', [], 0, scope)
    scope
  end

  defp loop(server) do
    receive do
      { :eval, ^server, code, config } ->
        # TODO: how should we handle helpers? 
        #       it would be better if they were evaluated in the same process.
        config = compile_do code, config, fn(forms) -> 
          pid = spawn fn ->
            PIDTable.start(self, config.binding, helpers_scope(config.scope))
            case Runner.next(forms) do
              { :ok, result } ->
                IO.puts "(#{inspect config.counter})#{inspect self} => #{inspect result}"
              { :exception, kind, reason, stacktrace } ->
                Evaluator.print_error(kind, reason, stacktrace)
            end
            PIDTable.finish(self)
          end

          IO.puts "(#{inspect config.counter})#{inspect pid}"
          config.update_counter(&(&1+1)).cache('').result(nil)
        end

        server <- { :evaled, self, config }
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
end
