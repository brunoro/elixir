defmodule Foo do
  import Debugger

  defdebug bar do
    pid = spawn fn ->
      receive do
        { from, :msg } ->
          from <- :ack
      end
    end
    pid <- { self, :msg }
    receive do
      :ack -> 
        :ok
      other ->
        other
    end
  end

  defdebug dup(x) do
    x * 2
  end
end

#IO.inspect Foo.dup(2)
IO.inspect Foo.bar
