# rebar3_go 🚀

A rebar3 plugin for managing Go modules in Erlang/OTP applications.

[![License](https://img.shields.io/github/license/mochams/rebar3_go)](https://github.com/mochams/rebar3_go)

## Features

- Adds Go modules to your Erlang/OTP project
- Compiles Go modules in your project
- Formats Go code in your project
- Tests Go modules in your project (Coming soon)
- Supports standalone and release/umbrella applications
- Minimal configuration required

## Requirements

- Erlang/OTP
- Go

## Installation

```erlang
%% Add the following to your rebar.config. Do NOT forget to replace the tag with the latest version.
{plugins, [
    {rebar3_go, {git, "https://github.com/mochams/rebar3_go", {tag, "v0.1.0"}}} 
]}.

%% Optional: Add the following to your rebar.config
{provider_hooks, [
    {pre, [
        {compile, {go, compile}}
    ]}
]}.
```

## Usage

```bash
# To add a Go module to your project (Standalone applications)
rebar3 go add -m <module_name>

# To add a Go module to your project (Release/Umbrella applications)
rebar3 go add -m <module_name> -a <app_name>

# To compile go modules
rebar3 go compile

# To format go code
rebar3 go fmt
```

## Directory Structure

### Standalone App

```bash
.
├── src                 # Erlang source code    
├── go.work             # Go workspace definition
├── go_src              # Go source code
│   ├── module1         # Go module 1
│   │   ├── main.go
│   │   └── go.mod
│   └── module2         # Go module 2
│       ├── main.go
│       └── go.mod
├── priv                # Priv directory
│   └── go              # Compiled Go code
│       ├── module1     # Compiled Go module 1
│       └── module2     # Compiled Go module 2
```

### Release/Umbrella App

```bash
.
├── apps
│   ├── app1                    # App 1
│   │   ├── src                 # Erlang source code
│   │   ├── go.work             # Go workspace definition
│   │   ├── go_src              # Go source code
│   │   │   ├── module1         # Go module 1
│   │   │   │   ├── main.go
│   │   │   │   └── go.mod
│   │   │   └── module2         # Go module 2
│   │   │       ├── main.go
│   │   │       └── go.mod
│   │   └── priv                # Priv directory
│   │       └── go              # Compiled Go code
│   │           ├── module1     # Compiled Go module 1
│   │           └── module2     # Compiled Go module 2
│   └── app2                    # App 2
│       ├── src                 # Erlang source code
│       ├── go.work             # Go workspace definition
│       ├── go_src              # Go source code
│       │   ├── module1         # Go module 1
│       │   │   ├── main.go
│       │   │   └── go.mod
│       │   └── module2         # Go module 2
│       │       ├── main.go
│       │       └── go.mod
│       └── priv                # Priv directory
│           └── go              # Compiled Go code
│               ├── module1     # Compiled Go module 1
│               └── module2     # Compiled Go module 2
└── rebar.config
```

## Commands

### Add Module Command

0. The plugin adds a Go module to the project.

    `Syntax(Standalone app)`

    ```bash
    rebar3 go add -m <module_name>
    ```

    `Syntax(Release/Umbrella app)`

    ```bash
    rebar3 go add -m <module_name> -a <app_name>
    ```

1. First time adding a Go module to your project, the plugin creates a `go.work` file in the app root directory.

    > In standalone applications, the `go.work` file will be created in the root directory of the application.  
    > In release/umbrella applications, the `go.work` file will be created in the root directory of the specified app.

2. The plugin creates a `go_src/<module_name>` directory and creates the `main.go` and `go.mod` files in it.

3. The plugin adds the module name to the `go.work` file.

### Compile Command

0. The plugin compiles the Go modules in the project.

    `Syntax`

    ```bash
    rebar3 go compile
    ```

1. The plugins goes through all applications in the project and checks if they have a `go_src` directory.

2. If a `go_src` directory is found, the plugin compiles the Go module(s) in the directory else it skips the application.

3. The plugin compiles the Go module(s) and copies the compiled code to the `priv/go` directory.

## Roadmap

- [x] Add module command
- [x] Compile command
- [x] Format command
- [ ] Test module command

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Acknowledgments

Inspired by the needs of the Erlang/OTP community for reliable Go port communication. See also [erlgo](https://github.com/mochams/erlgo)

Now go forth and make your Erlang and Go code talk to each other like old friends! 🚀✨
