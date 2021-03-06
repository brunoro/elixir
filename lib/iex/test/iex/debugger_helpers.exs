Code.require_file "../test_helper.exs", __DIR__

defmodule IEx.Debugger.HelpersTest do
  use IEx.Case

  import IEx.Helpers

  test "dc helper" do
    assert_raise UndefinedFunctionError, "undefined function: Sample.run/0", fn ->
      Sample.run
    end

    filename = "sample.ex"
    with_file filename, test_module_code, fn ->
      assert dc(filename) == [Sample]
      assert Sample.run == :run
    end
  after
    cleanup_modules([Sample])
  end

  test "add/remove debug patterns" do
    pattern = %r/.* - .*/ 
    assert dpg == []
    assert dps([pattern]) == [pattern]
    assert dpg == [pattern]
  end
end
