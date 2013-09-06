defmodule IEx.Debugger do
  use Application.Behaviour
  alias IEx.Debugger.Runner

  def start(opts) do
    IEx.Debugger.Supervisor.start_link(opts)
  end

  def stop do
    IEx.Debugger.Supervisor.stop
  end

  # for testing purposes
  defmacro defdebug(header, do: body) do
    wrapped_body = IEx.Debugger.wrap_quoted(body)
    quote do
      def unquote(header) do
        unquote(wrapped_body)
      end
    end
  end

  def debug_compile(source, path) do
    File.open source, [:read], fn(file) ->
      Enum.reduce(IO.stream(file, :line), [], &([&1 | &2]))
      |> Enum.reverse
      |> iolist_to_binary 
      |> Code.string_to_quoted(file: source)
      # TODO: we should handle errors from string_to_quoted!!
      |> Runner.strip_status 
      |> wrap_quoted
      |> List.wrap
      |> :elixir_compiler.quoted_to_path(source, path)
    end
  end

  def wrap_quoted({ :defmodule, meta, right }) do
    wrap_do = Enum.map right, fn
      list when is_list(list) ->
        case Keyword.get(list, :do) do
          do_block ->
            Keyword.put(list, :do, wrap_quoted(do_block))
          nil ->
            list
        end
      expr -> expr
    end
    { :defmodule, meta, wrap_do }
  end
  def wrap_quoted({ :fn, meta, clauses }) do
    { :fn, meta, Enum.map(clauses, Runner.wrap_next_call(&1)) }
  end
  def wrap_quoted({ :def, meta, right }) do
    [header, [do: body]] = right
    { :def, meta, [header, [do: Runner.wrap_next_call(body)]] }
  end
  def wrap_quoted({ left, meta, right }) when is_list(right) do
    { left, meta, Enum.map(right, &wrap_quoted/1) }
  end
  def wrap_quoted({ left, meta, right }) do
    { left, meta, wrap_quoted(right) }
  end
  def wrap_quoted(expr), do: expr
end
