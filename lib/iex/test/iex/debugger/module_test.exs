Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.ModuleTest do
  use ExUnit.Case, async: false
  import IEx.Debugger

  # test from kernel, if it compiles we're good
  defrecord State, a: nil

  defmodule B do
    defrecord State, b: nil
    def get(State[b: b]), do: b
  end

  def get(State[a: a]), do: a

  defdebugmodule Nested do
    defmodule A do
      def foo(x) do
        B.bar(x) * 2
      end
    end

    defmodule B do
      def bar(x) do
        x * x
      end
    end
    def foobar(x) do
      A.foo(x) + 1
    end
  end

  test "nested modules" do
    assert Nested.B.bar(3) == 9
    assert Nested.A.foo(3) == 18
    assert Nested.foobar(3) == 19
  end

  defdebugmodule Private do
    defp priv(x) do
      x * 2
    end

    def pub(x) do
      priv(x) + 1
    end
  end

  test "private functions" do
    assert Private.pub(2) == 5
    assert_raise UndefinedFunctionError, 
      "undefined function: IEx.Debugger.ModuleTest.Private.priv/1", fn ->
      Private.priv(2)
    end
  end
end

