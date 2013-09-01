defmodule Foo do
  import Debugger

  defdebug bar do
    try do
      :foo.bar
    rescue
      exp ->
        try do
          throw exp
        catch 
          :error, :undef ->
            :exp
        end
    end
  end

  defdebug baz do
    case true do
      false -> x = 1
      _ -> true
    end
    x
  end

  defdebug try_f1 do
    try do
      :foo.bar
    catch
      :error, :undef -> 2
    end
  end

  defdebug try_f2 do
    try do
      x = 1 + "a"
    rescue
      ArithmeticError -> 1
    end
  end

  # rescue only runtime errors
  defdebug try_rescue_f1 do
    try do
      raise "some error"
    rescue
      RuntimeError -> :rescue
    end
  end
  # rescue runtime and argument errors
  defdebug try_rescue_f2 do
    try do
      raise "some error"
    rescue
      [RuntimeError, ArgumentError] -> :rescue
    end
  end
  # rescue and assign to x
  defdebug try_rescue_f3 do
    try do
      raise "message"
    rescue
      x in [RuntimeError] ->
        # all exceptions have a message
        x.message
    end
  end

  defdebug anon do
    fun = fn(x) -> x + 1 end
    fun.(1)
  end
end

#IO.inspect Foo.try_rescue_f3
#IO.inspect Foo.try_f2
IO.inspect Foo.anon
