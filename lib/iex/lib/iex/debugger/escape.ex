defmodule IEx.Debugger.Escape do

  @pid_prefix "__PID_"
  @fun_prefix "__FUN_"
  @ref_prefix "__REF_"
  @port_prefix "__PORT_"

  defmacro is_escapable(thing) do
    quote do
      is_pid(unquote(thing)) 
      or is_function(unquote(thing))
      or is_reference(unquote(thing))
      or is_port(unquote(thing))
    end
  end

  def is_escaped?(bin) do
    String.starts_with?(bin, @pid_prefix) or
    String.starts_with?(bin, @fun_prefix) or
    String.starts_with?(bin, @ref_prefix) or
    String.starts_with?(bin, @port_prefix) 
  end

  # #PID<0.49.0> -> :__PID_0_49_0__
  def escape(pid) when is_pid(pid) do
    esc_pid = pid |> :erlang.pid_to_list
                  |> to_string
                  |> String.replace("<", "")
                  |> String.replace(">", "")
                  |> String.replace(".", "_") 

    "#{@pid_prefix}#{esc_pid}__"
  end

  # :"-expr/5-fun-2-" -> "__FUN_expr_5_fun_2__"
  def escape(fun) when is_function(fun) do
    fun_info = :erlang.fun_info(fun)
    fun_name = fun_info[:name] |> to_string
                               |> String.strip(?-)
                               |> String.replace("-", "_")
                               |> String.replace("/", "_")


    "#{@fun_prefix}#{fun_name}_"
  end

  # #Reference<0.0.0.76> -> :__REF_0_0_0_76__
  def escape(ref) when is_reference(ref) do
    esc_ref = ref |> :erlang.ref_to_list
                  |> to_string
                  |> String.replace("#Ref<", "")
                  |> String.replace(">", "")
                  |> String.replace(".", "_") 

    "#{@ref_prefix}#{esc_ref}__"
  end

  # #Port<0.1287> -> :__PORT_0_1287__
  def escape(port) when is_port(port) do
    esc_ref = port |> :erlang.port_to_list
                   |> to_string
                   |> String.replace("#Port<", "")
                   |> String.replace(">", "")
                   |> String.replace(".", "_") 

    "#{@port_prefix}#{esc_ref}__"
  end
end
