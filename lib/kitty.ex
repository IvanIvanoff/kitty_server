defmodule Kitty do
  @enforce_keys [:name]
  defstruct [:name, :color, :description]
end
