# banshee — VS Code extension

A thin client that runs the [`banshee`](../../README.md) language server
(`banshee lsp`) for PostgreSQL SQL files. It provides diagnostics, completion,
hover, formatting, document symbols, semantic highlighting, signature help, and
quick-fix code actions.

This extension is shipped as source for internal use; it is not published to
the Marketplace.

## Prerequisites

Build the `banshee` binary (with the `db` feature if you want live, schema-aware
diagnostics and completion):

```sh
cargo build --release -p banshee --features db
```

Put it on your `PATH`, or set `banshee.path` in your VS Code settings to the built
binary (e.g. `target/release/banshee`).

## Run it

From this directory:

```sh
npm install
npm run compile
```

Then in VS Code, open this folder and press **F5** to launch an Extension
Development Host. Open any `.sql` file and you should see diagnostics; trigger
completion with <kbd>Ctrl</kbd>+<kbd>Space</kbd> and quick-fixes via the
lightbulb.

To install a packaged build instead of the debug host:

```sh
npx vsce package        # produces banshee-vscode-0.1.0.vsix
code --install-extension banshee-vscode-0.1.0.vsix
```

## Settings

- `banshee.path` — path to the `banshee` executable (default: `banshee`).
- `banshee.trace.server` — LSP trace verbosity (`off` | `messages` | `verbose`).

Schema-aware features (column/table completion, JSONB key completion, `RF*`
reference checks) require a `banshee.toml` with a `[database]` section and a
`banshee` binary built with `--features db`. See the top-level README.
