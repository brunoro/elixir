defrecord IEx.Config, binding: nil, cache: '', counter: 1, prefix: "iex", scope: nil

defmodule IEx.Server do
  @moduledoc false

  alias IEx.Config

  @doc """
  Finds where the current IEx server is located.
  """
<<<<<<< HEAD
  @spec whereis :: pid | nil
  def whereis() do
    # Locate top group leader, always registered as user
    # can be implemented by group (normally) or user
    # (if oldshell or noshell)
    if user = Process.whereis(:user) do
      case :group.interfaces(user) do
        [] -> # Old or no shell
          case :user.interfaces(user) do
            [] -> nil
            [shell: shell] -> shell
          end
        [user_drv: user_drv] -> # Get current group from user_drv
          case :user_drv.interfaces(user_drv) do
            [] -> nil
            [current_group: group] -> :group.interfaces(group)[:shell]
          end
      end
=======
  def start(config) do
    # Disable ANSI-escape-sequence-based coloring on Windows
    # Can be overriden in .iex
    if match?({ :win32, _ }, :os.type()) do
      IEx.Options.set :colors, enabled: false
    end

    { _, _, scope } = :elixir.eval('require IEx.Helpers', [], 0, config.scope)
    config = config.scope(scope)

    config = case config.dot_iex_path do
      ""   -> config                     # don't load anything
      nil  -> load_dot_iex(config)       # load .iex from predefined locations
      path -> load_dot_iex(config, path) # load from `path`
>>>>>>> Reader <-> Server <-> Evaluator on IEx
    end
  end

<<<<<<< HEAD
  @doc """
  Requests to take over the given shell from the
  current process.
  """
  @spec take_over(binary, Keyword.t, pos_integer) ::
        :ok | { :error, :no_iex } | { :error, :refused }
  def take_over(identifier, opts, timeout // 1000, server // whereis()) do
    cond do
      nil?(server) ->
        { :error, :no_iex }
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
                IEx.Evaluator.start(server, leader)
            end
        after
          timeout ->
            { :error, :no_iex }
        end
=======
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"

    old_flag = Process.flag(:trap_exit, true)

    self_pid  = self
    input_pid = spawn_link fn -> input_loop(self_pid) end
    eval_pid  = spawn_link fn -> 
      # history should be initialized by evaluator process
      IEx.History.init
      eval_loop(self_pid) 
>>>>>>> Reader <-> Server <-> Evaluator on IEx
    end
  end

  @doc """
  Starts IEx by executing a given callback and spawning
  the server only after the callback is done.

  The server responsibilities include:

<<<<<<< HEAD
  * reading input
  * sending messages to the evaluator
  * handling takeover process of the evaluator

  If there is any takeover during the callback execution
  we spawn a new server for it without waiting for its
  conclusion.
  """
  @spec start(list, fun) :: :ok
  def start(opts, callback) do
    { pid, ref } = Process.spawn_monitor(callback)
    start_loop(opts, pid, ref)
  end

  defp start_loop(opts, pid, ref) do
=======
  defp eval_loop(config) do
    prefix = config.cache != []
    config.input_pid <- { :do_input, self, prefix, config.counter }
    wait_event(config)
  end

  defp wait_event(config) do
    pid = config.input_pid
>>>>>>> Reader <-> Server <-> Evaluator on IEx
    receive do
      { :take?, other, ref } ->
        other <- ref
        start_loop(opts, pid, ref)

      { :take, other, identifier, ref, opts } ->
        if allow_take?(identifier) do
          other <- { ref, Process.group_leader }
          run(opts)
        else
          other <- { ref, nil }
          start_loop(opts, pid, ref)
        end

      { :DOWN, ^ref, :process, ^pid,  :normal } ->
        run(opts)

      { :DOWN, ^ref, :process, ^pid,  _reason } ->
        :ok
<<<<<<< HEAD
    end
  end

  # Run loop: this is where the work is really
  # done after the start loop.

  defp run(opts) when is_list(opts) do
    IO.puts "Interactive Elixir (#{System.version}) - press Ctrl+C to exit (type h() ENTER for help)"
    self_pid    = self
    self_leader = Process.group_leader
    evaluator   = opts[:evaluator] || spawn(fn -> IEx.Evaluator.start(self_pid, self_leader) end)
    Process.put(:evaluator, evaluator)
    loop(run_config(opts), evaluator, Process.monitor(evaluator))
=======

      # REPL output
      { :break, new_config } ->
        do_loop(new_config)
      { :more, new_config } ->
        do_loop(new_config)
      { :ok, new_config } ->
        do_loop(new_config)
      { :error, _kind, _error, _stacktrace } ->
        do_loop(config.cache(''))

      # TODO: debugger events

      # exit from input or eval
      { :EXIT, _pid, :normal } ->
        wait_event(config)
      { :EXIT, pid, reason } ->
        print_exit(pid, reason)
        wait_event(config)
      other ->
        IO.puts :stderr, "#{inspect self} other: #{inspect other}"
    end
  end

  # Instead of doing just `:elixir.eval`, we first parse the expression to see
  # if it's well formed. If parsing succeeds, we evaluate the AST as usual.
  #
  # If parsing fails, this might be a TokenMissingError which we treat in
  # a special way (to allow for continuation of an expression on the next
  # line in IEx). In case of any other error, we let :elixir_translator
  # to re-raise it.
  #
  # Returns updated config.
  #
  # The first two clauses provide support for the break-trigger allowing to
  # break out from a pending incomplete expression. See
  # https://github.com/elixir-lang/elixir/issues/1089 for discussion.
  #
  @break_trigger '#iex:break\n'

  defp eval(_, @break_trigger, _, config=IEx.Config[cache: '']) do
    { :break, config }
  end

  defp eval(_, @break_trigger, line_no, _) do
    :elixir_errors.parse_error(line_no, "iex", 'incomplete expression', [])
  end

  defp eval(code_so_far, latest_input, line_no, config) do
    code = code_so_far ++ latest_input

    case :elixir_translator.forms(code, line_no, "iex", []) do
      { :ok, forms } ->
        { result, new_binding, scope } =
          :elixir.eval_forms(forms, config.binding, config.scope)

        unless result == IEx.dont_display_result, do: io_put result

        config = config.cache(code).scope(nil).result(result)
        update_history(config)
        new_config = config.update_counter(&(&1 + 1)).cache('')
                           .binding(new_binding).scope(scope).result(nil)
        { :ok, new_config }

      { :error, { line_no, error, token } } ->
        if token == [] do
          # Update config.cache so that IEx continues to add new input to
          # the unfinished expression in `code`
          { :more, config.cache(code) }
        else
          # Encountered malformed expression
          :elixir_errors.parse_error(line_no, "iex", error, token)
        end
    end
>>>>>>> Reader <-> Server <-> Evaluator on IEx
  end

  defp reset_loop(opts, evaluator, evaluator_ref) do
    exit_loop(evaluator, evaluator_ref, opts[:evaluator] != evaluator)
    IO.write [IO.ANSI.home, IO.ANSI.clear]
    run(opts)
  end

  defp exit_loop(evaluator, evaluator_ref, done? // true) do
    Process.delete(:evaluator)
    Process.demonitor(evaluator_ref)
    if done? do
      evaluator <- { :done, self }
    end
    :ok
  end

  defp loop(config, evaluator, evaluator_ref) do
    self_pid = self()
    counter  = config.counter
    prefix   = if config.cache != [], do: "...", else: config.prefix

    input = spawn(fn -> io_get(self_pid, prefix, counter) end)
    wait_input(config, evaluator, evaluator_ref, input)
  end

  defp wait_input(config, evaluator, evaluator_ref, input) do
    receive do
      # Input handling.
      # Message either go back to the main loop or exit.
      { :input, ^input, code } when is_binary(code) ->
        evaluator <- { :eval, self, code, config }
        wait_eval(evaluator, evaluator_ref)
      { :input, ^input, { :error, :interrupted } } ->
        io_error "** (EXIT) interrupted"
        loop(config.cache(''), evaluator, evaluator_ref)
      { :input, ^input, :eof } ->
        exit_loop(evaluator, evaluator_ref)
      { :input, ^input, { :error, :terminated } } ->
        exit_loop(evaluator, evaluator_ref)

      # Take process.
      # The take? message is received out of band, so we can
      # go back to wait for the same input. The take message
      # needs to take hold of the IO, so it kills the input,
      # re-runs the server OR goes back to the main loop.
      { :take?, other, ref } ->
        other <- ref
        wait_input(config, evaluator, evaluator_ref, input)
      { :take, other, identifier, ref, opts } ->
        kill_input(input)

        if allow_take?(identifier) do
          other <- { ref, Process.group_leader }
          reset_loop(opts, evaluator, evaluator_ref)
        else
          other <- { ref, nil }
          loop(config, evaluator, evaluator_ref)
        end

      # Evaluator handling.
      # We always kill the input to run a new one or to exit for real.
      { :respawn, ^evaluator } ->
        kill_input(input)
        IO.puts("")
        reset_loop([], evaluator, evaluator_ref)
      { :DOWN, ^evaluator_ref, :process, ^evaluator,  reason } ->
        io_error "** (EXIT from #{config.prefix} #{inspect evaluator}) #{inspect(reason)}"
        kill_input(input)
        exit_loop(evaluator, evaluator_ref)
    end
  end

  defp wait_eval(evaluator, evaluator_ref) do
    receive do
      { :evaled, ^evaluator, config } ->
        loop(config, evaluator, evaluator_ref)
      { :take?, other, ref } ->
        other <- ref
        wait_eval(evaluator, evaluator_ref)
      { :take, other, identifier, ref, opts } ->
        if allow_take?(identifier) do
          other <- { ref, Process.group_leader }
          reset_loop(opts, evaluator, evaluator_ref)
        else
          other <- { ref, nil }
          wait_eval(evaluator, evaluator_ref)
        end
    end
  end

  defp kill_input(input) do
    Process.exit(input, :kill)
  end

  defp allow_take?(identifier) do
    message = IEx.color(:eval_interrupt, "#{identifier}. Allow? [Yn] ")
    IO.gets(:stdio, message) =~ %r/^(Y(es)?)?$/i
  end

  ## Config

  defp run_config(opts) do
    locals = Keyword.get(opts, :delegate_locals_to, IEx.Helpers)

    scope =
      if env = opts[:env] do
        scope = :elixir_env.ex_to_scope(env)
        :elixir.scope_for_eval(scope, delegate_locals_to: locals)
      else
        :elixir.scope_for_eval(file: "iex", delegate_locals_to: locals)
      end
      eval_dot_iex(config, path)
    end
  end
  
  ## IO

  defp io_get(pid, prefix, counter) do
    prompt =
      if is_alive do
        "#{prefix || remote_prefix}(#{node})#{counter}> "
      else
        "#{prefix || "iex"}(#{counter})> "
      end

    pid <- { :input, self, IO.gets(:stdio, prompt) }
  end

  defp io_error(result) do
    IO.puts :stdio, IEx.color(:eval_error, result)
  end

  defp remote_prefix do
    if node == node(Process.group_leader), do: "iex", else: "rem"
  end
end
