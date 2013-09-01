defmodule Foo do
  import Debugger

  defdebug bar do
    try do
      :ok
    else
      :ok -> :cool
    catch
      kind, reason ->
        { kind, reason }
    end
  end
end

IO.inspect Foo.bar

