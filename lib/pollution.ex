defmodule Pollution do
  def start(_type, _args) do
    :pollution_sup.start_link()
  end
end
