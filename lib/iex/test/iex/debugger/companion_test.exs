Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.CompanionTest do
  use ExUnit.Case, async: false
  alias IEx.Debugger.Companion
  alias IEx.Debugger.Evaluator
  alias IEx.Debugger.State

  test "put/get state" do
    binding = [a: 1]
    env = __ENV__
    { :ok, comp } = Companion.start_link(binding, env)

    state = Companion.get_state(comp)

    same_env = Evaluator.update_binding(env, binding)
    same_state = State[binding: binding, env: same_env]
    assert state.binding == same_state.binding

    new_binding = [a: 1, b: 2]
    new_env = Evaluator.update_binding(env, new_binding)
    new_state = state.binding(new_binding).env(new_env)
    Companion.put_state(comp, new_state)

    assert new_state.binding == Companion.get_state(comp).binding
  end

  test "push/pop state stack" do
    binding = [a: 1]
    env = __ENV__
    { :ok, comp } = Companion.start_link(binding, env)

    state = Companion.get_state(comp)
    Companion.push_stack(comp, state.binding, state.env)
    push_state = Companion.get_state(comp)

    assert state.binding == push_state.binding
    assert state.stack != push_state.stack

    Companion.put_state(comp, push_state.binding([a: 1, b: 2]).env(env))
    new_state = Companion.get_state(comp)
    assert new_state.binding[:b] == 2

    Companion.pop_stack(comp)
    old_state = Companion.get_state(comp)

    assert state == old_state
    assert old_state.binding[:b] == nil
    
    # can't pop more the stack
    assert Companion.pop_stack(comp) == :done
  end
end
