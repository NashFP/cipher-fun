defmodule Cipher.Rot13 do
  @doc ~S"""
  iex> Cipher.Rot13.rot13("The Quick Brown Fox Jumps Over The Lazy Dog.")
  "Gur Dhvpx Oebja Sbk Whzcf Bire Gur Ynml Qbt."

  iex> Cipher.Rot13.rot13(v)
  "The Quick Brown Fox Jumps Over The Lazy Dog."
  """
  def rot13(input) do
    input
    |> to_charlist()
    |> Enum.map(&rotate/1)
    |> to_string()
  end

  defp rotate(char) do
    cond do
      char in ?A..?M or char in ?a..?m ->
        char + 13

      char in ?N..?Z or char in ?n..?z ->
        char - 13

      true ->
        char
    end
  end
end
