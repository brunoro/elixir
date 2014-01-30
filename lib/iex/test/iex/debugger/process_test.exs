Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.ProcessTest do
  use ExUnit.Case, async: false
  import IEx.Debugger
  
  defdebug spawn_f1 do
    pid = spawn fn -> self end
    pid != self
  end

  defdebug spawn_f2 do
    pid = spawn_link fn -> self end
    pid != self
  end

  test "spawning processes" do
    assert spawn_f1 == true
    assert spawn_f2 == true
  end

  defdebug msg_f1 do
    this = self
    _pid = spawn fn ->
      send this, :msg
    end
    receive do
      :msg -> :ok
    after
      200 -> :fail
    end
  end
  defdebug msg_f2 do
    pid = spawn fn ->
      receive do
        { from, :msg } ->
          send from, :ack
      after
        200 -> :fail
      end
    end
    send pid, { self, :msg }
    receive do
      :ack -> :ok
    after
        200 -> :fail
    end
  end

  test "message passing" do
    assert msg_f1 == :ok
    assert msg_f2 == :ok
  end
end
