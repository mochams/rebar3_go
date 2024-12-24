# rebar3_go 🚀

Because your Erlang needs some Go-Go juice! A rebar3 plugin that manages Go ports like a boss.

## What's This Magic?

You've got Erlang. You've got Go. You want them to be besties. We get it! This plugin makes your Go ports play nice with your Erlang/OTP application. No manual compilation, no path juggling, just pure inter-language harmony.

## Features

- Automatically compiles Go ports to the correct location
- Handles multiple Go modules
- Minimal configuration required

## Quick Start

1. Add to your `rebar.config`:

    ```erlang
    {plugins, [
        {rebar3_go, {git, "https://github.com/mochams/rebar3_go", {branch, "develop"}}}
    ]}.

    {provider_hooks, [
        {pre, [
            {compile, {go, compile}}
        ]}
    ]}.
    ```

2. Watch the magic happen:

    ```bash
    # Compile go modules
    rebar3 go compile

    # Compile erlang and go modules
    rebar3 compile
    ```

## But How?

1. Your Go code resides in `go_src/`
2. We compile it to `priv/go/` with help of `go workspaces`
3. Your Erlang code talks to it via ports ([erlgo](https://github.com/mochams/erlgo))
4. Everyone's happy!

## Requirements

- Erlang/OTP
- Go

## Contributing

Found a bug? Want to add something cool? PRs are welcome!

## Acknowledgments

- The creators of Erlang and Rebar3
- The Go team
- You, for reading this far! 🌟

Remember: In a world of microservices, we chose port communication. We're either brave or crazy. Probably both.

Now go forth and make your Erlang and Go code talk to each other like old friends! 🚀✨
