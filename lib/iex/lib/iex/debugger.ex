defmodule Debugger do
  use Application.Behaviour

  alias Debugger.Runner

  def start(_type, _args) do
    Debugger.Supervisor.start_link
  end

  def compile(paths) do
    Enum.map paths, fn(path) ->
      File.open path, [:read], fn(file) ->
        Enum.reduce(IO.stream(file, :line), [], &([&1 | &2]))
        |> Enum.reverse
        |> iolist_to_binary 
        |> Code.string_to_quoted(file: path)
        # TODO: we should handle errors from string_to_quoted!!
        |> Runner.strip_status 
        |> wrap_quoted
        |> Code.compile_quoted
      end
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
