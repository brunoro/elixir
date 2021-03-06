Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.EscapeTest do
  use ExUnit.Case, async: false
  import IEx.Debugger.Escape

  test "escape pid" do
    pid = self
    esc = escape(pid)
    assert true == is_escaped? esc
  end
  
  test "escape references" do
    ref = make_ref
    esc = escape(ref)
    assert true == is_escaped? esc
  end
  
  test "escape ports" do
    port = self
    esc = escape(port)
    assert true == is_escaped? esc
  end

  test "escape functions" do
    fun = fn(x) -> x end
    esc = escape(fun)
    assert true == is_escaped? esc
  end
end
