Code.require_file "test_helper.exs", __DIR__

defmodule IEx.Debugger.CompanionTest do
  use ExUnit.Case, async: false
  alias IEx.Debugger.Companion
  alias IEx.Debugger.State

  test "put/get state" do
    binding = [a: 1]
    scope = :elixir_scope.to_erl_env(__ENV__)
    { :ok, state_server } = Companion.start_link(binding, scope)

    state = Companion.get_state(state_server)
    same_scope = :elixir_scope.vars_from_binding(scope, binding)
    same_state = State[binding: binding, scope: same_scope]
    assert state == same_state

    new_binding = [a: 1, b: 2]
    new_scope = :elixir_scope.vars_from_binding(scope, new_binding)
    new_state = state.binding(new_binding).scope(new_scope)
    Companion.put_state(state_server, new_state)

    assert new_state == Companion.get_state(state_server)
  end

  test "push/pop state stack" do
    binding = [a: 1]
    scope = :elixir_scope.to_erl_env(__ENV__)
    { :ok, state_server } = Companion.start_link(binding, scope)

    state = Companion.get_state(state_server)
    Companion.push_stack(state_server)
    push_state = Companion.get_state(state_server)

    assert state.binding == push_state.binding
    assert state.scope == push_state.scope
    assert state.stack != push_state.stack

    Companion.put_state(state_server, push_state.binding([a: 1, b: 2]).scope(scope))
    new_state = Companion.get_state(state_server)
    assert new_state.binding[:b] == 2

    Companion.pop_stack(state_server)
    old_state = Companion.get_state(state_server)

    assert state == old_state
    assert old_state.binding[:b] == nil
    
    # can't pop more the stack
    Companion.pop_stack(state_server)
    assert old_state == Companion.get_state(state_server)
  end
end
