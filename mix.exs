defmodule Hackney.Mixfile do
use Mix.Project

    def project do
        [
            app: :hackney,
            version: "1.5.4",
            description: "simple HTTP client for the Erlang VM",
            deps: deps,
            package: package,
            language: :erlang
        ]
    end

    def application do
        [
            mod: {:hackney_app, []}
        ]
    end

    def deps do
        [
            {:idna, "~> 1.2.0"},
            {:mimerl, "~> 1.0.2"},
            {:certifi, "~> 0.4.0"},
            {:metrics, "~> 1.0.1"},
            {:ssl_verify_hostname, "1.0.6"}
        ]
    end

    defp package do
        [
            files: [
                "src",
                "include",
                "mix.exs",
                "mix.lock",
                "rebar.config",
                "rebar.lock",
                "README.md",
                "NEWS.md",
                "LICENSE",
                "NOTICE",
                "MAINTAINERS"
            ],
            maintainers: ["Benoit Chesneau", "Eduardo Gurgel"],
            licenses: ["Apache 2.0"],
            links: %{"Github" => "https://github.com/benoitc/hackney"}
        ]
    end
end
