defmodule IEx.Debugger do
  use Application.Behaviour
  alias IEx.Debugger.Runner

  def start(opts),        do: IEx.Debugger.Supervisor.start_link(opts)
  def start(_type, opts), do: IEx.Debugger.Supervisor.start_link(opts)

  def stop, do: IEx.Debugger.Supervisor.stop

  # for testing purposes
  defmacro defdebug(header, do: body) do
    wrapped_body = Runner.wrap_next_clause(body)
    quote do
      def unquote(header) do
        IEx.Debugger.PIDTable.start_link
        IEx.Debugger.Controller.start_link
        unquote(wrapped_body)
      end
    end
  end
  defmacro defdebugmodule(name, do: contents) do
    source = __FILE__
    wrapped_contents = wrap_quoted(contents)
    quote do
      defmodule unquote(name) do 
        unquote(wrapped_contents)
      end
    end
  end
 

  defp string_to_quoted!(input, source) do
    code = String.to_char_list!(input)
    case :elixir_translator.forms(code, 0, source, []) do
      { :ok, forms } ->
        forms
      { :error, { line, error, token } } ->
        :elixir_errors.parse_error(line, source, error, token)
    end
  end

  def debug_compile(source, path) do
    File.open source, [:read], fn(file) ->
      Enum.reduce(IO.stream(file, :line), [], &([&1 | &2]))
      |> Enum.reverse
      |> iolist_to_binary 
      |> string_to_quoted!(source)
      |> wrap_quoted
      |> List.wrap
      |> :elixir_compiler.quoted_to_path(source, path)
    end
  end

  def wrap_quoted({ :defmodule, meta, right }) do
    wrap_do = Enum.map right, fn
      list when is_list(list) ->
        case Keyword.get(list, :do) do
          nil ->
            list
          do_block ->
            Keyword.put(list, :do, wrap_quoted(do_block))
        end
      expr -> 
        wrap_quoted(expr)
    end
    { :defmodule, meta, wrap_do }
  end
  def wrap_quoted({ :def, meta, right }) do
    [header, [do: body]] = right
    { :def, meta, [header, [do: Runner.wrap_next_clause(body)]] }
  end
  # TODO: check calling context for private functions
  def wrap_quoted({ :defp, meta, right }) do
    [header, [do: body]] = right
    { :def, meta, [header, [do: Runner.wrap_next_clause(body)]] }
  end
  def wrap_quoted({ left, meta, right }) when is_list(right) do
    { left, meta, Enum.map(right, &wrap_quoted(&1)) }
  end
  def wrap_quoted(expr_list) when is_list(expr_list) do
    Enum.map(expr_list, &wrap_quoted(&1))
  end
  def wrap_quoted({ left, meta, right }) do
    { left, meta, wrap_quoted(right) }
  end
  def wrap_quoted(expr), do: expr
end
