Code.require_file "test_helper.exs", __DIR__
import IEx.Debugger

defrecord RecordTest.SomeRecord, a: 0, b: 1
defrecord RecordTest.WithNoField, []

## Dynamic names and overridable
name = RecordTest.DynamicName
defrecord name, a: 0, b: 1 do
  defoverridable [update_b: 2]

  def update_b(_, _) do
    :not_optimizable
  end

  Record.import __MODULE__, as: :self
end

defdebugmodule RecordTest.DynamicOpts do
  @a [foo: 1..30]
  defrecord State, (lc {name, _interval} inlist @a, do: {name, nil})
end

## With types

defrecord RecordTest.WithTypeOverriden, a: 0, b: 1 do
  @type t :: __MODULE__[a: integer, b: any]
end

defrecord RecordTest.WithRecordType, a: 0, b: 1 do
  record_type a: non_pos_integer
  record_type a: integer
end

defdebugmodule RecordTest.Macros do
  defmacro gen do
    quote do
      alias RecordTest.Macros.Nested

      def this_works, do: RecordTest.Macros.Nested[]
      def this_should_too, do: Nested[]
    end
  end

  defrecord Nested do
    def nested_record_alias?(Nested[]) do
      true
    end

    defrecord NestedInNested, it_compiles: true
  end

  # Ensure there is no conflict in a nested module
  # named record.
  defrecord Record, [a: 1, b: 2]
end

defmodule IEx.Debugger.RecordTest do
  use ExUnit.Case, async: true

  # Check the access from the generated macro works
  # as expected. If it compiles, we are good to go.
  require RecordTest.Macros
  RecordTest.Macros.gen

  defdebug dynamic_name_new(args // []) do
    RecordTest.DynamicName.new(args)
  end

  test :dynamic_record_name do
    record = dynamic_name_new
    assert record.a == 0
    assert record.b == 1
  end

  test :dynamic_update do
    record = dynamic_name_new
    assert record.update_a(10 + &1).a == 10
  end

  test :is_record do
    assert is_record(RecordTest.WithNoField.new)
    refute is_record(empty_tuple)
    refute is_record(a_list)
  end

  test :__record_index__ do
    record = dynamic_name_new(a: "a", b: "b")
    assert record.__record__(:index, :a) == 1
    assert elem(record, record.__record__(:index, :a)) == "a"
    assert elem(record, record.__record__(:index, :b)) == "b"
    assert record.__record__(:index, :c) == nil
  end

  test :to_keywords do
    record = dynamic_name_new(a: "a", b: "b")
    assert record.to_keywords[:a] == "a"
    assert record.to_keywords[:b] == "b"
  end

  test :record_update do
    record = RecordTest.SomeRecord.new
    assert RecordTest.SomeRecord.a(record.update(a: 2, b: 3)) == 2
    assert RecordTest.SomeRecord.b(record.update(a: 2, b: 3)) == 3
    assert RecordTest.SomeRecord.a(record.update(a: 2)) == 2
    assert RecordTest.SomeRecord.b(record.update(b: 2)) == 2
  end

  test :optimizable do
    assert { :b, 1 } in RecordTest.SomeRecord.__record__(:optimizable)
    assert { :b, 2 } in RecordTest.SomeRecord.__record__(:optimizable)
    assert { :update_b, 2 } in RecordTest.SomeRecord.__record__(:optimizable)
    refute { :update_b, 2 } in RecordTest.DynamicName.__record__(:optimizable)
  end

  test :result do
    assert { :module, _, _, "result"} = (defrecord WithResult, foo: :bar do
      "result"
    end)
  end

  test :record_with_functions_as_defaults do
    defrecord WithFun, fun: [&Kernel.is_atom/1]
    assert WithFun.new.fun == [&Kernel.is_atom/1]

    assert_raise ArgumentError, "record field default value :bar can only contain functions that point to an existing &Mod.fun/arity", fn ->
      defrecord Foo, bar: [fn x -> x end]
    end
  end

  test :extract_with_nested_records do
    namespace = Record.extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl")[:namespace]
    assert is_record(namespace, :xmlNamespace)
  end

  defp empty_tuple, do: {}
  defp a_list,  do: [ :foo, :bar, :baz ]
end
