defmodule IEx.Debugger.Supervisor do
  alias IEx.Debugger.PIDTable
  use Supervisor.Behaviour

  def start_link(opts) do
    :supervisor.start_link(__MODULE__, opts)
  end

  def init(opts) do
    children = [
      # Define workers and child supervisors to be supervised
      worker(IEx.Debugger.PIDTable, []),
      worker(IEx.Debugger.Controller, [opts[:reader] || nil])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end
end
