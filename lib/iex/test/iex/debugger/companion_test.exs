Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.CompanionTest do
  use ExUnit.Case, async: false
  alias IEx.Debugger.Companion
  alias IEx.Debugger.Evaluator
  alias IEx.Debugger.State

  test "put/get state" do
    binding = [a: 1]
    scope = :elixir_env.ex_to_scope(__ENV__)
    { :ok, comp } = Companion.start_link(binding, scope)

    state = Companion.get_state(comp)

    same_scope = Evaluator.update_binding(scope, binding)
    same_state = State[binding: binding, scope: same_scope]
    assert state.binding == same_state.binding

    new_binding = [a: 1, b: 2]
    new_scope = Evaluator.update_binding(scope, new_binding)
    new_state = state.binding(new_binding).scope(new_scope)
    Companion.put_state(comp, new_state)

    assert new_state.binding == Companion.get_state(comp).binding
  end

  test "push/pop state stack" do
    binding = [a: 1]
    scope = :elixir_env.ex_to_scope(__ENV__)
    { :ok, comp } = Companion.start_link(binding, scope)

    state = Companion.get_state(comp)
    Companion.push_stack(comp, state.binding, state.scope)
    push_state = Companion.get_state(comp)

    assert state.binding == push_state.binding
    assert state.stack != push_state.stack

    Companion.put_state(comp, push_state.binding([a: 1, b: 2]).scope(scope))
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
