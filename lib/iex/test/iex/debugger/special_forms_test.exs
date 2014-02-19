Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.SpecialFormsTest do
  use ExUnit.Case, async: false
  import IEx.Debugger
  
  #@special_forms [:__ENV__, :__MODULE__, :__DIR__]

  #Enum.map @special_forms, fn(form) ->
  #end
  defdebugmodule SpecialForms do
    def env,    do: __ENV__ 
    def module, do: __MODULE__
    def dir,    do: __DIR__ 
  end

  test "__ENV__, __MODULE__, __DIR__" do
    env  = SpecialForms.env
    mod  = SpecialForms.module 
    dir  = SpecialForms.dir

    assert mod  == IEx.Debugger.SpecialFormsTest.SpecialForms
    assert dir  == __DIR__

    assert is_record(env, Macro.Env)
    assert env.module == mod
  end
end
