defmodule NdIndex.Mixfile do
  use Mix.Project

  def project do
    [app: :nd_index,
     version: "1.7.1",
     language: :erlang,
     description: description(),
     package: package(),
     deps: deps()]
  end

  def application do
    []
  end

  defp deps do
    []
  end

  defp description do
    "Erlang N-dimensional Index Iterator"
  end

  defp package do
    [files: ~w(src test rebar.config README.markdown LICENSE),
     maintainers: ["Michael Truog"],
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/okeuday/nd_index"}]
   end
end
