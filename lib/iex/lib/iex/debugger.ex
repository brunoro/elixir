defmodule IEx.Debugger do
  use Application.Behaviour
  alias IEx.Debugger.Runner

  def start do
    IEx.Debugger.Supervisor.start_link
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

  def wrap_quoted({ :fn, meta, [do: block] }) do
    { :fn, meta, [do: Runner.wrap_next_call(block)] }
  end
  def wrap_quoted({ left, meta, right }) when is_list(right) do
    { left, meta, Enum.map(right, &wrap_quoted/1) }
  end
  def wrap_quoted({ left, meta, right }) do
    { left, meta, wrap_quoted(right) }
  end
  def wrap_quoted(expr), do: expr
end
